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
let time_of_ms ms = Time_ns.of_int_ns_since_epoch (ms * 1_000_000)

(* One instrument and last top of book per coin, found from the coin's bytes in a frame
   without building a string. *)
module Coins = struct
  type entry =
    { name : string
    ; instrument : Md.Instrument.t
    ; mutable last_top : Md.Bbo.t option
    }

  type t =
    { table : entry list array
    ; mutable all : entry list
    }

  let buckets = 256
  let create () = { table = Array.create ~len:buckets []; all = [] }

  (* FNV-1a over the bytes (offset basis cut to 63 bits), folded into a bucket. *)
  let[@zero_alloc] hash s ~pos ~len =
    let mutable h = 0x4bf29ce484222325 in
    for i = pos to pos + len - 1 do
      h <- h lxor Char.to_int (String.unsafe_get s i) * 0x100000001b3
    done;
    h land (buckets - 1)
  ;;

  let rec find_in bucket s ~pos ~len =
    match bucket with
    | [] -> None
    | e :: rest ->
      if Frame_scan.equal_sub s ~pos ~len e.name then Some e else find_in rest s ~pos ~len
  ;;

  (* Allocation-free on a hit: [find_in] is inlined into the match. *)
  let find t s ~pos ~len =
    let b = hash s ~pos ~len in
    match find_in t.table.(b) s ~pos ~len with
    | Some e -> e
    | None ->
      let name = String.sub s ~pos ~len in
      let e = { name; instrument = instrument name; last_top = None } in
      t.table.(b) <- e :: t.table.(b);
      t.all <- e :: t.all;
      e
  ;;

  let of_name t name = find t name ~pos:0 ~len:(String.length name)
  let reset t = List.iter t.all ~f:(fun e -> e.last_top <- None)
end

(* The Yojson path: every frame the scanner refuses, and every [activeAssetCtx] frame. *)

let trade coins ~recv_ts (t : Trade.t) : Md.Trade.t =
  { instrument = (Coins.of_name coins (Coin.to_string t.coin)).instrument
  ; exch_ts = time_of_ms t.time
  ; recv_ts
  ; price = t.px
  ; size = t.sz
  ; side = t.side
  ; trade_id = Int.to_string t.tid
  }
;;

let level (l : L2_book.Level.t) : Md.L2_update.Level.t = { price = l.px; size = l.sz }

let l2 instrument ~recv_ts (book : L2_book.t) : Md.L2_update.t =
  { instrument
  ; exch_ts = time_of_ms book.time
  ; recv_ts
  ; kind = Snapshot
  ; bids = List.map book.bids ~f:level
  ; asks = List.map book.asks ~f:level
  }
;;

let bbo instrument ~recv_ts (book : L2_book.t) : Md.Bbo.t option =
  match L2_book.best_bid book, L2_book.best_ask book with
  | Some bid, Some ask ->
    Some
      { instrument
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
let funding coins ~recv_ts data : Md.Funding.t option =
  let open Yojson.Safe.Util in
  let ctx = member "ctx" data in
  match member "funding" ctx with
  | `Null -> None
  | rate ->
    let price field = to_string (member field ctx) |> Price.of_string in
    Some
      { instrument = (Coins.of_name coins (to_string (member "coin" data))).instrument
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
  ; trades : bool
  ; l2 : bool
  ; bbo : bool
  ; coins : Coins.t
  ; trade_scan : Frame_scan.Trade.t
  ; book_scan : Frame_scan.Book.t
  }

let create ?(streams = Md.Stream.[ Trades; Bbo; L2; Funding ]) () =
  { streams
  ; trades = wants streams Trades
  ; l2 = wants streams L2
  ; bbo = wants streams Bbo
  ; coins = Coins.create ()
  ; trade_scan = Frame_scan.Trade.create ()
  ; book_scan = Frame_scan.Book.create ()
  }
;;

let reset t = Coins.reset t.coins

(* Every book change moves the l2Book channel, mostly below the top; only a changed top of
   book is a new bbo. *)
let same_top (a : Md.Bbo.t) (b : Md.Bbo.t) =
  Md.Price.equal a.bid_price b.bid_price
  && Md.Size.equal a.bid_size b.bid_size
  && Md.Price.equal a.ask_price b.ask_price
  && Md.Size.equal a.ask_size b.ask_size
;;

let new_top (coin : Coins.entry) (x : Md.Bbo.t) =
  match coin.last_top with
  | Some last when same_top last x -> None
  | _ ->
    coin.last_top <- Some x;
    Some (Md.Event.Bbo x)
;;

let decode_yojson t ~recv_ts frame =
  match Ws_message.t_of_yojson (Yojson.Safe.from_string frame) with
  | Trades trades when t.trades ->
    List.map trades ~f:(fun x -> Md.Event.Trade (trade t.coins ~recv_ts x))
  | L2_book book ->
    let coin = Coins.of_name t.coins (Coin.to_string book.coin) in
    (* Computed before the bbo table is touched, so a frame that raises changes nothing. *)
    let top = if t.bbo then bbo coin.instrument ~recv_ts book else None in
    let snapshot = Option.some_if t.l2 (Md.Event.L2 (l2 coin.instrument ~recv_ts book)) in
    List.filter_opt [ snapshot; Option.bind top ~f:(new_top coin) ]
  | Unknown { channel = "activeAssetCtx"; data } when wants t.streams Funding ->
    funding t.coins ~recv_ts data
    |> Option.map ~f:(fun f -> Md.Event.Funding f)
    |> Option.to_list
  | _ -> []
;;

let frame_yojson t ~recv_ts frame =
  match
    streams_of_channel ~streams:t.streams (Option.value (channel frame) ~default:"")
  with
  | [] -> Ok []
  | _ :: _ -> Or_error.try_with (fun () -> decode_yojson t ~recv_ts frame)
;;

(* The scanner path: [trades] and [l2Book] frames read by {!Frame_scan}, events built
   straight from the scanned units. [Refused] sends the frame to the Yojson path. *)

exception Refused

let scan_trades t ~recv_ts frame =
  let s = t.trade_scan in
  let rec loop p =
    if p = -1
    then []
    else (
      let stop =
        if p >= 0 && Char.equal (String.unsafe_get frame p) '{'
        then Frame_scan.Trade.read_at frame p s
        else -1
      in
      if stop < 0
      then raise_notrace Refused
      else (
        let coin = Coins.find t.coins frame ~pos:s.coin_pos ~len:s.coin_len in
        let event =
          Md.Event.Trade
            { instrument = coin.instrument
            ; exch_ts = time_of_ms s.time
            ; recv_ts
            ; price = Md.Price.of_units s.px
            ; size = Md.Size.of_units s.sz
            ; side = (if s.buy then Buy else Sell)
            ; trade_id = Int.to_string s.tid
            }
        in
        event :: loop (Frame_scan.next_element frame stop)))
  in
  loop (Frame_scan.first_element frame (Frame_scan.trades_data frame))
;;

let levels px sz n : Md.L2_update.Level.t list =
  let mutable acc = [] in
  for i = n - 1 downto 0 do
    acc
    <- ({ price = Md.Price.of_units (Array.unsafe_get px i)
        ; size = Md.Size.of_units (Array.unsafe_get sz i)
        }
        : Md.L2_update.Level.t)
       :: acc
  done;
  acc
;;

let unchanged_top (coin : Coins.entry) (b : Frame_scan.Book.t) =
  match coin.last_top with
  | None -> false
  | Some last ->
    Md.Price.to_units last.bid_price = b.bid_px.(0)
    && Md.Size.to_units last.bid_size = b.bid_sz.(0)
    && Md.Price.to_units last.ask_price = b.ask_px.(0)
    && Md.Size.to_units last.ask_size = b.ask_sz.(0)
;;

let scan_book t ~recv_ts frame =
  let b = t.book_scan in
  (* Only the top level is read when the l2 snapshot is not wanted. *)
  let depth = if t.l2 then Frame_scan.Book.capacity else 1 in
  if not (Frame_scan.Book.read ~depth frame b) then raise_notrace Refused;
  let coin = Coins.find t.coins frame ~pos:b.coin_pos ~len:b.coin_len in
  let exch_ts = time_of_ms b.time in
  let top =
    if (not t.bbo) || b.n_bids = 0 || b.n_asks = 0 || unchanged_top coin b
    then []
    else (
      let x : Md.Bbo.t =
        { instrument = coin.instrument
        ; exch_ts
        ; recv_ts
        ; bid_price = Md.Price.of_units b.bid_px.(0)
        ; bid_size = Md.Size.of_units b.bid_sz.(0)
        ; ask_price = Md.Price.of_units b.ask_px.(0)
        ; ask_size = Md.Size.of_units b.ask_sz.(0)
        }
      in
      coin.last_top <- Some x;
      [ Md.Event.Bbo x ])
  in
  if t.l2
  then
    Md.Event.L2
      { instrument = coin.instrument
      ; exch_ts
      ; recv_ts
      ; kind = Snapshot
      ; bids = levels b.bid_px b.bid_sz b.n_bids
      ; asks = levels b.ask_px b.ask_sz b.n_asks
      }
    :: top
  else top
;;

let frame t ~recv_ts frame =
  match Frame_scan.channel frame with
  | Trades when not t.trades -> Ok []
  | Trades ->
    (match scan_trades t ~recv_ts frame with
     | events -> Ok events
     | exception Refused -> frame_yojson t ~recv_ts frame)
  | L2_book when not (t.l2 || t.bbo) -> Ok []
  | L2_book ->
    (match scan_book t ~recv_ts frame with
     | events -> Ok events
     | exception Refused -> frame_yojson t ~recv_ts frame)
  | Active_asset_ctx | Other -> frame_yojson t ~recv_ts frame
;;

module For_testing = struct
  let frame_yojson = frame_yojson
end
