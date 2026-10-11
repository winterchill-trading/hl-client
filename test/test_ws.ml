open! Core
open Hl_protocol
open Hl_client
module T = Ws.For_testing

let%expect_test "base64" =
  List.iter [ ""; "f"; "fo"; "foo"; "foob"; "the sample nonce" ] ~f:(fun s ->
    print_endline (T.base64 s));
  [%expect
    {|

    Zg==
    Zm8=
    Zm9v
    Zm9vYg==
    dGhlIHNhbXBsZSBub25jZQ==
    |}]
;;

let%expect_test "handshake request" =
  print_string
    (T.handshake_request
       (Uri.of_string "ws://localhost:3000/ws")
       ~key:"dGhlIHNhbXBsZSBub25jZQ==");
  print_string
    (T.handshake_request (Uri.of_string "wss://api.hyperliquid.xyz/ws") ~key:"k");
  [%expect
    {|
    GET /ws HTTP/1.1
    Host: localhost:3000
    Upgrade: websocket
    Connection: Upgrade
    Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
    Sec-WebSocket-Version: 13

    GET /ws HTTP/1.1
    Host: api.hyperliquid.xyz
    Upgrade: websocket
    Connection: Upgrade
    Sec-WebSocket-Key: k
    Sec-WebSocket-Version: 13
    |}]
;;

let%expect_test "handshake response (RFC 6455 example)" =
  let key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let check status_line headers =
    print_s
      [%sexp (T.check_handshake_response ~key ~status_line ~headers : unit Or_error.t)]
  in
  check
    "HTTP/1.1 101 Switching Protocols"
    [ "upgrade", " websocket"; "sec-websocket-accept", " s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" ];
  check "HTTP/1.1 101 Switching Protocols" [ "Sec-WebSocket-Accept", "wrong" ];
  check "HTTP/1.1 404 Not Found" [];
  [%expect
    {|
    (Ok ())
    (Error
     ("bad Sec-WebSocket-Accept" (accept (wrong))
      (expected s3pPLMBiTxaQ9kYGzzhZRbK+xOo=)))
    (Error ("websocket upgrade refused" (status_line "HTTP/1.1 404 Not Found")))
    |}]
;;

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

let%expect_test "backoff" =
  print_s
    [%sexp
      (T.backoff
         ~initial:(Time_float.Span.of_sec 0.5)
         ~max:(Time_float.Span.of_sec 5.)
         ~attempts:7
       : Time_float.Span.t list)];
  [%expect {| (500ms 1s 2s 4s 5s 5s 5s) |}]
;;
