open! Core
open Hl_client
module Md = Market_data
module T = Live_feed.For_testing

let all : Md.Stream.t list = [ Trades; Bbo; L2; Funding ]
let recv_ts = Time_ns.of_string_with_utc_offset "2023-11-14 22:13:20.5Z"

let show ?(streams = all) frame =
  match T.normalize ~streams ~recv_ts frame with
  | Ok events -> List.iter events ~f:(fun e -> print_s [%sexp (e : Md.Event.t)])
  | Error _ -> print_endline "<unparsable>"
;;

let%expect_test "subscribe requests" =
  List.iter (T.subscribe_requests ~symbols:[ "BTC"; "ETH" ] ~streams:all) ~f:print_endline;
  [%expect
    {|
    {"method":"subscribe","subscription":{"type":"trades","coin":"BTC"}}
    {"method":"subscribe","subscription":{"type":"l2Book","coin":"BTC"}}
    {"method":"subscribe","subscription":{"type":"activeAssetCtx","coin":"BTC"}}
    {"method":"subscribe","subscription":{"type":"trades","coin":"ETH"}}
    {"method":"subscribe","subscription":{"type":"l2Book","coin":"ETH"}}
    {"method":"subscribe","subscription":{"type":"activeAssetCtx","coin":"ETH"}}
    |}];
  (* Bbo and L2 share one l2Book subscription. *)
  List.iter
    (T.subscribe_requests ~symbols:[ "BTC" ] ~streams:[ Bbo; L2 ])
    ~f:print_endline;
  [%expect {| {"method":"subscribe","subscription":{"type":"l2Book","coin":"BTC"}} |}]
;;

let%expect_test "channel scan" =
  List.iter
    [ {|{"channel":"trades","data":[]}|}
    ; {|{ "channel" : "l2Book", "data": {}}|}
    ; {|{"data":{},"channel":"activeAssetCtx"}|}
    ; {|{"channel":"pong"}|}
    ; {|{"channel":42}|}
    ; "not json"
    ]
    ~f:(fun frame -> print_s [%sexp (T.channel frame : string option)]);
  [%expect
    {|
    (trades)
    (l2Book)
    (activeAssetCtx)
    (pong)
    ()
    ()
    |}];
  List.iter [ "trades"; "l2Book"; "activeAssetCtx"; "pong" ] ~f:(fun channel ->
    print_s
      [%sexp
        (channel : string)
        , (T.streams_of_channel ~streams:[ Trades; Bbo ] channel : Md.Stream.t list)]);
  [%expect
    {|
    (trades (Trades))
    (l2Book (Bbo))
    (activeAssetCtx ())
    (pong ())
    |}]
;;

let%expect_test "trades" =
  show
    {|{"channel":"trades","data":[{"coin":"BTC","side":"A","px":"113000.5","sz":"0.01","hash":"0x0000000000000000000000000000000000000000000000000000000000000000","time":1700000000000,"tid":42,"users":["0x0000000000000000000000000000000000000001","0x0000000000000000000000000000000000000002"]},{"coin":"BTC","side":"B","px":"113001","sz":"0.5","hash":"0x00","time":1700000000001,"tid":43,"users":["0x0000000000000000000000000000000000000001","0x0000000000000000000000000000000000000002"]}]}|};
  [%expect
    {|
    (Trade
     ((instrument ((venue Hyperliquid) (symbol BTC)))
      (exch_ts (2023-11-14 22:13:20.000000000Z))
      (recv_ts (2023-11-14 22:13:20.500000000Z)) (price 113000.5) (size 0.01)
      (side Sell) (trade_id 42)))
    (Trade
     ((instrument ((venue Hyperliquid) (symbol BTC)))
      (exch_ts (2023-11-14 22:13:20.001000000Z))
      (recv_ts (2023-11-14 22:13:20.500000000Z)) (price 113001) (size 0.5)
      (side Buy) (trade_id 43)))
    |}]
;;

let book =
  {|{"channel":"l2Book","data":{"coin":"BTC","time":1700000000000,"levels":[[{"px":"100","sz":"1","n":1},{"px":"99.5","sz":"3","n":2}],[{"px":"101","sz":"2","n":2}]]}}|}
;;

let%expect_test "l2Book gives a snapshot and the top of book" =
  show book;
  [%expect
    {|
    (L2
     ((instrument ((venue Hyperliquid) (symbol BTC)))
      (exch_ts (2023-11-14 22:13:20.000000000Z))
      (recv_ts (2023-11-14 22:13:20.500000000Z)) (kind Snapshot)
      (bids (((price 100) (size 1)) ((price 99.5) (size 3))))
      (asks (((price 101) (size 2))))))
    (Bbo
     ((instrument ((venue Hyperliquid) (symbol BTC)))
      (exch_ts (2023-11-14 22:13:20.000000000Z))
      (recv_ts (2023-11-14 22:13:20.500000000Z)) (bid_price 100) (bid_size 1)
      (ask_price 101) (ask_size 2)))
    |}]
;;

let%expect_test "l2Book with only bbo requested, and a one-sided book" =
  show ~streams:[ Bbo ] book;
  show
    ~streams:[ Bbo; L2 ]
    {|{"channel":"l2Book","data":{"coin":"ETH","time":1700000000000,"levels":[[],[{"px":"101","sz":"2","n":2}]]}}|};
  [%expect
    {|
    (Bbo
     ((instrument ((venue Hyperliquid) (symbol BTC)))
      (exch_ts (2023-11-14 22:13:20.000000000Z))
      (recv_ts (2023-11-14 22:13:20.500000000Z)) (bid_price 100) (bid_size 1)
      (ask_price 101) (ask_size 2)))
    (L2
     ((instrument ((venue Hyperliquid) (symbol ETH)))
      (exch_ts (2023-11-14 22:13:20.000000000Z))
      (recv_ts (2023-11-14 22:13:20.500000000Z)) (kind Snapshot) (bids ())
      (asks (((price 101) (size 2))))))
    |}]
;;

let%expect_test "activeAssetCtx" =
  show
    {|{"channel":"activeAssetCtx","data":{"coin":"BTC","ctx":{"funding":"0.0000125","openInterest":"25000.1","prevDayPx":"112000.0","dayNtlVlm":"1234567890.0","premium":"0.0001","oraclePx":"113010.0","markPx":"113020.0","midPx":"113015.5","impactPxs":["113015.0","113016.0"],"dayBaseVlm":"11000.0"}}}|};
  (* Spot contexts carry no funding. *)
  show
    {|{"channel":"activeAssetCtx","data":{"coin":"@107","ctx":{"prevDayPx":"40.0","dayNtlVlm":"1.0","markPx":"41.0","midPx":"41.0","circulatingSupply":"1.0","coin":"@107"}}}|};
  [%expect
    {|
    (Funding
     ((instrument ((venue Hyperliquid) (symbol BTC)))
      (exch_ts (2023-11-14 22:13:20.500000000Z))
      (recv_ts (2023-11-14 22:13:20.500000000Z)) (rate 1.25E-05)
      (next_funding_time (2023-11-14 23:00:00.000000000Z)) (mark_price 113020)
      (index_price 113010)))
    |}]
;;

let%expect_test "other channels, unrequested streams and bad frames" =
  show {|{"channel":"pong"}|};
  show
    {|{"channel":"subscriptionResponse","data":{"method":"subscribe","subscription":{"type":"trades","coin":"BTC"}}}|};
  show ~streams:[ Funding ] book;
  show {|{"channel":"trades","data":"garbage"}|};
  show {|{"channel":"l2Book","data":{"coin":"BTC"}}|};
  show {|{"channel":"activeAssetCtx","data":{"coin":"BTC","ctx":{"funding":"x"}}}|};
  [%expect {|
    <unparsable>
    <unparsable>
    <unparsable>
    |}]
;;

let%expect_test "raw frames are recorded under one stream" =
  List.iter
    [ [ Md.Stream.Bbo; L2 ]; [ Bbo ]; [ L2 ]; [ Trades ] ]
    ~f:(fun streams ->
      print_s
        [%sexp
          (streams : Md.Stream.t list)
          , (T.raw_stream ~streams "l2Book" : Md.Stream.t option)
          , (T.raw_stream ~streams "trades" : Md.Stream.t option)]);
  [%expect
    {|
    ((Bbo L2) (L2) ())
    ((Bbo) (Bbo) ())
    ((L2) (L2) ())
    ((Trades) () (Trades))
    |}]
;;
