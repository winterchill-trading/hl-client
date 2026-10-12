(* [Normalizer] over the first recorded frames of each stream, plus the top-of-book
   de-duplication and the frames it ignores. *)

open! Core
open Hl_client
module Md = Market_data

let frames stream =
  In_channel.read_lines
    (sprintf
       "fixtures/raw/hyperliquid/%s/BTC/2026/10/12/13-35.ndjson"
       (Md.Stream.to_string stream))
  |> List.map ~f:(fun line ->
    let recv_ts, frame = String.lsplit2_exn line ~on:' ' in
    Time_ns.of_string_with_utc_offset recv_ts, frame)
;;

(* Times in UTC whatever the machine's zone; books summarized to their top level, a
   snapshot has 20 levels a side. *)
let utc time = Time_ns.to_string_iso8601_basic time ~zone:Time_float.Zone.utc

let sexp_of_event : Md.Event.t -> Sexp.t = function
  | Trade x ->
    [%sexp
      Trade
        { instrument = (Md.Instrument.to_string x.instrument : string)
        ; exch_ts = (utc x.exch_ts : string)
        ; recv_ts = (utc x.recv_ts : string)
        ; price : Md.Price.t = x.price
        ; size : Md.Size.t = x.size
        ; side : Md.Side.t = x.side
        ; trade_id : string = x.trade_id
        }]
  | Bbo x ->
    [%sexp
      Bbo
        { instrument = (Md.Instrument.to_string x.instrument : string)
        ; exch_ts = (utc x.exch_ts : string)
        ; recv_ts = (utc x.recv_ts : string)
        ; bid = ((x.bid_price, x.bid_size) : Md.Price.t * Md.Size.t)
        ; ask = ((x.ask_price, x.ask_size) : Md.Price.t * Md.Size.t)
        }]
  | L2 x ->
    let top (levels : Md.L2_update.Level.t list) =
      match levels with
      | [] -> [%sexp "empty"]
      | l :: _ -> [%sexp (l.price : Md.Price.t), (l.size : Md.Size.t)]
    in
    [%sexp
      L2
        { instrument = (Md.Instrument.to_string x.instrument : string)
        ; exch_ts = (utc x.exch_ts : string)
        ; recv_ts = (utc x.recv_ts : string)
        ; kind : Md.L2_update.Kind.t = x.kind
        ; bids = (List.length x.bids : int)
        ; asks = (List.length x.asks : int)
        ; best_bid = (top x.bids : Sexp.t)
        ; best_ask = (top x.asks : Sexp.t)
        }]
  | Funding x ->
    [%sexp
      Funding
        { instrument = (Md.Instrument.to_string x.instrument : string)
        ; exch_ts = (utc x.exch_ts : string)
        ; recv_ts = (utc x.recv_ts : string)
        ; rate : float = x.rate
        ; next_funding_time = (utc x.next_funding_time : string)
        ; mark : Md.Price.t = x.mark_price
        ; index : Md.Price.t = x.index_price
        }]
;;

let print_event event = print_s (sexp_of_event event)

let feed normalizer (recv_ts, frame) =
  match Normalizer.frame normalizer ~recv_ts frame with
  | Ok events -> List.iter events ~f:print_event
  | Error error -> print_s [%message "error" (error : Error.t)]
;;

let first n stream = List.take (frames stream) n

let%expect_test "recorded trades" =
  List.iter (first 2 Trades) ~f:(feed (Normalizer.create ()));
  [%expect
    {|
    (Trade
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:00.812000000Z)
      (recv_ts 2026-10-12T13:35:01.515217000Z) (price 82525) (size 0.01671)
      (side Sell) (trade_id 112328450464875)))
    (Trade
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:00.812000000Z)
      (recv_ts 2026-10-12T13:35:01.515217000Z) (price 82525) (size 0.01669)
      (side Sell) (trade_id 170664730792116)))
    (Trade
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:01.517000000Z)
      (recv_ts 2026-10-12T13:35:01.907588000Z) (price 82525) (size 0.00075)
      (side Sell) (trade_id 648332236416894)))
    |}]
;;

let%expect_test "recorded books give a snapshot and a bbo" =
  List.iter (first 2 L2) ~f:(feed (Normalizer.create ()));
  [%expect
    {|
    (L2
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:03.008000000Z)
      (recv_ts 2026-10-12T13:35:03.466814000Z) (kind Snapshot) (bids 20)
      (asks 20) (best_bid (82514 0.73912)) (best_ask (82515 2.07099))))
    (Bbo
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:03.008000000Z)
      (recv_ts 2026-10-12T13:35:03.466814000Z) (bid (82514 0.73912))
      (ask (82515 2.07099))))
    (L2
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:08.453000000Z)
      (recv_ts 2026-10-12T13:35:08.855140000Z) (kind Snapshot) (bids 20)
      (asks 20) (best_bid (82492 12.37392)) (best_ask (82493 1.34562))))
    (Bbo
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:08.453000000Z)
      (recv_ts 2026-10-12T13:35:08.855140000Z) (bid (82492 12.37392))
      (ask (82493 1.34562))))
    |}]
;;

let%expect_test "recorded asset contexts give funding" =
  List.iter (first 2 Funding) ~f:(feed (Normalizer.create ()));
  [%expect
    {|
    (Funding
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:00.275883000Z)
      (recv_ts 2026-10-12T13:35:00.275883000Z) (rate 8.8036E-06)
      (next_funding_time 2026-10-12T14:00:00.000000000Z) (mark 82528)
      (index 82565.6)))
    (Funding
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:01.515341000Z)
      (recv_ts 2026-10-12T13:35:01.515341000Z) (rate 8.8233E-06)
      (next_funding_time 2026-10-12T14:00:00.000000000Z) (mark 82521)
      (index 82558)))
    |}]
;;

let%expect_test "an unchanged top of book gives no bbo until reset" =
  let normalizer = Normalizer.create ~streams:[ Bbo ] () in
  let book = List.hd_exn (frames L2) in
  feed normalizer book;
  [%expect
    {|
    (Bbo
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:03.008000000Z)
      (recv_ts 2026-10-12T13:35:03.466814000Z) (bid (82514 0.73912))
      (ask (82515 2.07099))))
    |}];
  feed normalizer book;
  [%expect {| |}];
  Normalizer.reset normalizer;
  feed normalizer book;
  [%expect
    {|
    (Bbo
     ((instrument hyperliquid:BTC) (exch_ts 2026-10-12T13:35:03.008000000Z)
      (recv_ts 2026-10-12T13:35:03.466814000Z) (bid (82514 0.73912))
      (ask (82515 2.07099))))
    |}]
;;

let%expect_test "unwanted, control and malformed frames" =
  let normalizer = Normalizer.create ~streams:[ Trades ] () in
  let at = Time_ns.epoch in
  List.iter
    [ List.hd_exn (frames L2) |> snd
    ; {|{"channel":"subscriptionResponse","data":{"method":"subscribe"}}|}
    ; {|{"channel":"pong"}|}
    ; {|{"channel":"trades","data":[{"coin":"BTC"}]}|}
    ]
    ~f:(fun frame -> feed normalizer (at, frame));
  [%expect
    {|
    (error
     (error
      ("Of_yojson_error (Failure(\"hl-protocol/lib/trade.ml.t_of_yojson: the following record elements were undefined: side px sz hash time tid users\"), {\"coin\":\"BTC\"})")))
    |}]
;;
