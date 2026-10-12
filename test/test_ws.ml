open! Core
open Hl_protocol
open Hl_client
module T = Ws.For_testing

let%expect_test "encode requests" =
  List.iter
    [ Ws_request.Subscribe (L2_book { coin = Coin.of_string "BTC" })
    ; Unsubscribe All_mids
    ; Ping
    ]
    ~f:(fun request -> print_endline (T.encode request));
  [%expect
    {|
    {"method":"subscribe","subscription":{"type":"l2Book","coin":"BTC"}}
    {"method":"unsubscribe","subscription":{"type":"allMids"}}
    {"method":"ping"}
    |}]
;;

let%expect_test "decode messages" =
  List.iter
    [ {|{"channel":"subscriptionResponse","data":{"method":"subscribe","subscription":{"type":"allMids"}}}|}
    ; {|{"channel":"allMids","data":{"mids":{"BTC":"113000.5"}}}|}
    ; {|{"channel":"pong"}|}
    ; {|{"channel":"somethingNew","data":[1,2]}|}
    ; {|{"channel":"l2Book","data":"garbage"}|}
    ; "not json"
    ]
    ~f:(fun frame ->
      (* Error texts embed source paths of hl-protocol, which differ between a workspace
         build and an opam install, so only report that decoding failed. *)
      match T.decode frame with
      | Ok message -> print_s [%sexp (message : Ws_message.t)]
      | Error _ -> print_endline "<undecodable>");
  [%expect
    {|
    (Subscription_response (Subscribe All_mids))
    (All_mids ((BTC 113000.5)))
    Pong
    (Unknown (channel somethingNew) (data [1,2]))
    <undecodable>
    <undecodable>
    |}]
;;
