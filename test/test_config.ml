open! Core
open Hl_client

let print t = print_s [%sexp (t : Config.t)]

let%expect_test "local defaults" =
  let t = Config.local () in
  print t;
  print_endline (Uri.to_string (Config.info_uri t));
  print_endline (Uri.to_string (Config.exchange_uri t));
  [%expect
    {|
    ((base_url http://localhost:3000) (ws_url ws://localhost:3000/ws)
     (address ()))
    http://localhost:3000/info
    http://localhost:3000/exchange
    |}]
;;

let%expect_test "https maps to wss and base paths are kept" =
  print (Config.mainnet ());
  print
    (Config.create
       ~address:
         (Hl_protocol.Address.of_string "0xABCDEF0000000000000000000000000000000001")
       ~base_url:(Uri.of_string "http://sim:8080/api/")
       ());
  print_endline
    (Uri.to_string
       (Config.info_uri
          (Config.create ~base_url:(Uri.of_string "http://sim:8080/api/") ())));
  [%expect
    {|
    ((base_url https://api.hyperliquid.xyz) (ws_url wss://api.hyperliquid.xyz/ws)
     (address ()))
    ((base_url http://sim:8080/api/) (ws_url ws://sim:8080/ws)
     (address (0xabcdef0000000000000000000000000000000001)))
    http://sim:8080/api/info
    |}]
;;
