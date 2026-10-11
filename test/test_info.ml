open! Core
open Hl_protocol
open Hl_client

let user = Address.of_string "0x0000000000000000000000000000000000000001"
let btc = Coin.of_string "BTC"

let%expect_test "request bodies" =
  List.iter
    [ Info_request.L2_book { coin = btc; n_sig_figs = None; mantissa = None }
    ; L2_book { coin = btc; n_sig_figs = Some 5; mantissa = Some 2 }
    ; All_mids
    ; Open_orders { user }
    ; User_fills { user }
    ; Clearinghouse_state { user }
    ; Meta
    ]
    ~f:(fun request -> print_endline (Yojson.Safe.to_string (Info.request_body request)));
  [%expect
    {|
    {"type":"l2Book","coin":"BTC"}
    {"type":"l2Book","coin":"BTC","nSigFigs":5,"mantissa":2}
    {"type":"allMids"}
    {"type":"openOrders","user":"0x0000000000000000000000000000000000000001"}
    {"type":"userFills","user":"0x0000000000000000000000000000000000000001"}
    {"type":"clearinghouseState","user":"0x0000000000000000000000000000000000000001"}
    {"type":"meta"}
    |}]
;;

let decode ?(status = 200) request body =
  print_s
    [%sexp
      (Info.decode request { Http.Response.status; body } : Info_response.t Or_error.t)]
;;

let%expect_test "decode responses" =
  decode
    (L2_book { coin = btc; n_sig_figs = None; mantissa = None })
    {|{"coin":"BTC","time":1754450974231,"levels":[[{"px":"113377.0","sz":"7.6699","n":17}],[{"px":"113378.0","sz":"0.5","n":2}]]}|};
  decode All_mids {|{"BTC":"113000.5","ETH":"4000"}|};
  decode
    (Open_orders { user })
    {|[{"coin":"BTC","side":"A","limitPx":"29792.0","sz":"0.5","oid":91490942,"timestamp":1681247412573,"origSz":"5.0"}]|};
  decode Meta {|{"universe":[{"name":"BTC","szDecimals":5,"maxLeverage":50}]}|};
  [%expect
    {|
    (Ok
     (L2_book
      ((coin BTC) (time 1754450974231) (bids (((px 113377) (sz 7.6699) (n 17))))
       (asks (((px 113378) (sz 0.5) (n 2)))))))
    (Ok (All_mids ((BTC 113000.5) (ETH 4000))))
    (Ok
     (Open_orders
      (((coin BTC) (side Sell) (limit_px 29792) (sz 0.5) (oid 91490942)
        (timestamp 1681247412573) (orig_sz (5)) (cloid ())))))
    (Ok
     (Meta
      ((universe
        (((name BTC) (sz_decimals 5) (max_leverage 50) (only_isolated false)))))))
    |}]
;;

let%expect_test "decode failures carry status and body" =
  decode All_mids ~status:500 "internal error";
  decode All_mids "not json";
  decode Meta {|{"universe":"nope"}|};
  [%expect
    {|
    (Error (((status 500) (body "internal error")) "unexpected HTTP status"))
    (Error
     (((status 200) (body "not json"))
      ("Yojson__Common.Json_error(\"Line 1, bytes 0-8:\\nInvalid token 'not json'\")")))
    (Error
     (((status 200) (body "{\"universe\":\"nope\"}"))
      ("Of_yojson_error (Failure(\"list_of_yojson: list needed\"), \"nope\")")))
    |}]
;;
