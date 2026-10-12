open! Core
open Hl_protocol
module Md = Market_data

let wants streams stream = List.mem streams stream ~equal:Md.Stream.equal

let channel frame =
  let open Option.Let_syntax in
  let%bind i = String.substr_index frame ~pattern:{|"channel"|} in
  let len = String.length frame in
  let rec skip j =
    if j < len && (Char.is_whitespace frame.[j] || Char.equal frame.[j] ':')
    then skip (j + 1)
    else j
  in
  let start = skip (i + String.length {|"channel"|}) in
  let%bind () = Option.some_if (start < len && Char.equal frame.[start] '"') () in
  let%map stop = String.index_from frame (start + 1) '"' in
  String.sub frame ~pos:(start + 1) ~len:(stop - start - 1)
;;

let streams_of_channel ~streams channel =
  let candidates : Md.Stream.t list =
    match channel with
    | "trades" -> [ Trades ]
    | "l2Book" -> [ L2; Bbo ]
    | "activeAssetCtx" -> [ Funding ]
    | _ -> []
  in
  List.filter candidates ~f:(wants streams)
;;

let instrument symbol = Md.Instrument.create Hyperliquid symbol
let instrument_of_coin coin = instrument (Coin.to_string coin)
let time_of_ms ms = Time_ns.of_int_ns_since_epoch (ms * 1_000_000)

let trade ~recv_ts (t : Trade.t) : Md.Trade.t =
  { instrument = instrument_of_coin t.coin
  ; exch_ts = time_of_ms t.time
  ; recv_ts
  ; price = t.px
  ; size = t.sz
  ; side = t.side
  ; trade_id = Int.to_string t.tid
  }
;;

let level (l : L2_book.Level.t) : Md.L2_update.Level.t = { price = l.px; size = l.sz }

let l2 ~recv_ts (book : L2_book.t) : Md.L2_update.t =
  { instrument = instrument_of_coin book.coin
  ; exch_ts = time_of_ms book.time
  ; recv_ts
  ; kind = Snapshot
  ; bids = List.map book.bids ~f:level
  ; asks = List.map book.asks ~f:level
  }
;;

let bbo ~recv_ts (book : L2_book.t) : Md.Bbo.t option =
  match L2_book.best_bid book, L2_book.best_ask book with
  | Some bid, Some ask ->
    Some
      { instrument = instrument_of_coin book.coin
      ; exch_ts = time_of_ms book.time
      ; recv_ts
      ; bid_price = bid.px
      ; bid_size = bid.sz
      ; ask_price = ask.px
      ; ask_size = ask.sz
      }
  | _ -> None
;;

let hour = Time_ns.Span.hour

let next_hour time =
  let since_epoch = Time_ns.to_span_since_epoch time in
  let hours = Time_ns.Span.(since_epoch // hour) |> Float.iround_down_exn in
  Time_ns.of_span_since_epoch (Time_ns.Span.scale_int hour (hours + 1))
;;

(* [{"coin": "BTC", "ctx": {"funding": "0.0000125", "markPx": "...", "oraclePx": "...", ...}}].
   Spot contexts have no funding and are skipped. *)
let funding ~recv_ts data : Md.Funding.t option =
  let open Yojson.Safe.Util in
  let ctx = member "ctx" data in
  match member "funding" ctx with
  | `Null -> None
  | rate ->
    let price field = to_string (member field ctx) |> Price.of_string in
    Some
      { instrument = instrument (to_string (member "coin" data))
      ; exch_ts = recv_ts
      ; recv_ts
      ; rate = Float.of_string (to_string rate)
      ; next_funding_time = next_hour recv_ts
      ; mark_price = price "markPx"
      ; index_price = price "oraclePx"
      }
;;

type t =
  { streams : Md.Stream.t list
  ; last_bbo : Md.Bbo.t Md.Instrument.Table.t
  }

let create ?(streams = Md.Stream.[ Trades; Bbo; L2; Funding ]) () =
  { streams; last_bbo = Md.Instrument.Table.create () }
;;

let reset t = Hashtbl.clear t.last_bbo

(* Every book change moves the l2Book channel, mostly below the top; only a changed top of
   book is a new bbo. *)
let same_top (a : Md.Bbo.t) (b : Md.Bbo.t) =
  Md.Price.equal a.bid_price b.bid_price
  && Md.Size.equal a.bid_size b.bid_size
  && Md.Price.equal a.ask_price b.ask_price
  && Md.Size.equal a.ask_size b.ask_size
;;

let new_top t (x : Md.Bbo.t) =
  match Hashtbl.find t.last_bbo x.instrument with
  | Some last when same_top last x -> None
  | _ ->
    Hashtbl.set t.last_bbo ~key:x.instrument ~data:x;
    Some (Md.Event.Bbo x)
;;

let decode t ~recv_ts frame =
  match Ws_message.t_of_yojson (Yojson.Safe.from_string frame) with
  | Trades trades when wants t.streams Trades ->
    List.map trades ~f:(fun x -> Md.Event.Trade (trade ~recv_ts x))
  | L2_book book ->
    (* Computed before the bbo table is touched, so a frame that raises changes nothing. *)
    let top = if wants t.streams Bbo then bbo ~recv_ts book else None in
    let snapshot = Option.some_if (wants t.streams L2) (Md.Event.L2 (l2 ~recv_ts book)) in
    List.filter_opt [ snapshot; Option.bind top ~f:(new_top t) ]
  | Unknown { channel = "activeAssetCtx"; data } when wants t.streams Funding ->
    funding ~recv_ts data |> Option.map ~f:(fun f -> Md.Event.Funding f) |> Option.to_list
  | _ -> []
;;

let frame t ~recv_ts frame =
  match
    streams_of_channel ~streams:t.streams (Option.value (channel frame) ~default:"")
  with
  | [] -> Ok []
  | _ :: _ -> Or_error.try_with (fun () -> decode t ~recv_ts frame)
;;
