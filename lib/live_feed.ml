open! Core
open! Async
open Hl_protocol
module Md = Market_data
module Ws_client = Venue_net.Ws_client

let wants streams stream = List.mem streams stream ~equal:Md.Stream.equal

let subscription_json ~type_ ~coin =
  `Assoc
    [ "method", `String "subscribe"
    ; "subscription", `Assoc [ "type", `String type_; "coin", `String coin ]
    ]
  |> Yojson.Safe.to_string
;;

(* Hl_protocol.Subscription has no activeAssetCtx, so all requests are built here. *)
let subscribe_requests ~symbols ~streams =
  let types =
    List.concat
      [ (if wants streams Trades then [ "trades" ] else [])
      ; (if wants streams L2 || wants streams Bbo then [ "l2Book" ] else [])
      ; (if wants streams Funding then [ "activeAssetCtx" ] else [])
      ]
  in
  List.concat_map symbols ~f:(fun coin ->
    List.map types ~f:(fun type_ -> subscription_json ~type_ ~coin))
;;

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

let instrument coin = Md.Instrument.create Hyperliquid (Coin.to_string coin)
let time_of_ms ms = Time_ns.of_int_ns_since_epoch (ms * 1_000_000)

let trade ~recv_ts (t : Trade.t) : Md.Trade.t =
  { instrument = instrument t.coin
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
  { instrument = instrument book.coin
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
      { instrument = instrument book.coin
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
      { instrument = Md.Instrument.create Hyperliquid (to_string (member "coin" data))
      ; exch_ts = recv_ts
      ; recv_ts
      ; rate = Float.of_string (to_string rate)
      ; next_funding_time = next_hour recv_ts
      ; mark_price = price "markPx"
      ; index_price = price "oraclePx"
      }
;;

let normalize ~streams ~recv_ts frame : Md.Event.t list Or_error.t =
  match streams_of_channel ~streams (Option.value (channel frame) ~default:"") with
  | [] -> Ok []
  | _ ->
    Or_error.try_with (fun () ->
      match Ws_message.t_of_yojson (Yojson.Safe.from_string frame) with
      | Trades trades when wants streams Trades ->
        List.map trades ~f:(fun t -> Md.Event.Trade (trade ~recv_ts t))
      | L2_book book ->
        List.filter_opt
          [ Option.some_if (wants streams L2) (Md.Event.L2 (l2 ~recv_ts book))
          ; (if wants streams Bbo
             then Option.map (bbo ~recv_ts book) ~f:(fun b -> Md.Event.Bbo b)
             else None)
          ]
      | Unknown { channel = "activeAssetCtx"; data } when wants streams Funding ->
        funding ~recv_ts data
        |> Option.map ~f:(fun f -> Md.Event.Funding f)
        |> Option.to_list
      | _ -> [])
;;

module Pipes = struct
  type t =
    { trades : Md.Trade.t Pipe.Writer.t
    ; bbo : Md.Bbo.t Pipe.Writer.t
    ; l2 : Md.L2_update.t Pipe.Writer.t
    ; funding : Md.Funding.t Pipe.Writer.t
    }

  let write t : Md.Event.t -> unit = function
    | Trade x -> Pipe.write_without_pushback_if_open t.trades x
    | Bbo x -> Pipe.write_without_pushback_if_open t.bbo x
    | L2 x -> Pipe.write_without_pushback_if_open t.l2 x
    | Funding x -> Pipe.write_without_pushback_if_open t.funding x
  ;;

  let close t =
    Pipe.close t.trades;
    Pipe.close t.bbo;
    Pipe.close t.l2;
    Pipe.close t.funding
  ;;
end

let create
  ?(on_raw = fun ~stream:_ ~recv_ts:_ _ -> ())
  (config : Config.t)
  ~symbols
  ~streams
  =
  let requests = subscribe_requests ~symbols ~streams in
  let on_connect client =
    Deferred.all_unit (List.map requests ~f:(Ws_client.send client))
  in
  let%map client =
    Ws_client.connect
      ~ping:(`Text ({|{"method":"ping"}|}, Time_ns.Span.of_sec 50.))
      ~max_silent_intervals:1
      ~on_connect
      ~log_name:"hl-client live feed"
      ~url:config.ws_url
      ()
    >>| ok_exn
  in
  let trades_r, trades = Pipe.create () in
  let bbo_r, bbo = Pipe.create () in
  let l2_r, l2 = Pipe.create () in
  let funding_r, funding = Pipe.create () in
  let pipes = { Pipes.trades; bbo; l2; funding } in
  if not (wants streams Trades) then Pipe.close trades;
  if not (wants streams Bbo) then Pipe.close bbo;
  if not (wants streams L2) then Pipe.close l2;
  if not (wants streams Funding) then Pipe.close funding;
  let on_frame frame =
    let recv_ts = Time_ns.now () in
    match channel frame with
    | Some "error" ->
      Log.Global.error_s [%message "hl-client live feed: error frame" (frame : string)]
    | channel ->
      (match streams_of_channel ~streams (Option.value channel ~default:"") with
       | [] -> ()
       | feeds ->
         List.iter feeds ~f:(fun stream -> on_raw ~stream ~recv_ts frame);
         (match normalize ~streams ~recv_ts frame with
          | Ok events -> List.iter events ~f:(Pipes.write pipes)
          | Error error ->
            Log.Global.error_s
              [%message
                "hl-client live feed: unparsable frame" (frame : string) (error : Error.t)]))
  in
  don't_wait_for
    (let%map () = Pipe.iter_without_pushback (Ws_client.messages client) ~f:on_frame in
     Pipes.close pipes);
  let module Feed = struct
    let trades = trades_r
    let bbo = bbo_r
    let l2 = l2_r
    let funding = funding_r

    let close () =
      let%map () = Ws_client.close client in
      Pipes.close pipes
    ;;
  end
  in
  (module Feed : Md.Feed.S)
;;

module For_testing = struct
  let subscribe_requests = subscribe_requests
  let channel = channel
  let streams_of_channel = streams_of_channel
  let normalize = normalize
end
