(* End-to-end test of [Ws] against a websocket server on 127.0.0.1, built from the same
   async_websocket library with [~role:Server]. *)

open! Core
open Async
open Hl_protocol
open Hl_client
module Server = Loopback_server

(* Waits for [values_available] rather than racing [Pipe.read] against the timeout: a
   timed out [Pipe.read] stays queued on the pipe and would swallow the next message. *)
let read_and_print messages n =
  Deferred.repeat_until_finished n (fun n ->
    if n = 0
    then return (`Finished ())
    else (
      match%map Clock.with_timeout (sec 5.) (Pipe.values_available messages) with
      | `Timeout ->
        print_endline "timeout";
        `Finished ()
      | `Result `Eof ->
        print_endline "eof";
        `Finished ()
      | `Result `Ok ->
        (match Pipe.read_now messages with
         | `Ok message -> print_s [%sexp (message : Ws_message.t)]
         | `Eof | `Nothing_available -> ());
        `Repeat (n - 1)))
;;

let%expect_test "connect, subscribe, reconnect and resubscribe" =
  Log.Global.set_output [];
  let%bind server = Server.start () in
  let config =
    Config.create
      ~base_url:
        (Uri.of_string
           (sprintf "http://127.0.0.1:%d" (Tcp.Server.listening_on server.tcp)))
      ()
  in
  let%bind ws =
    Ws.connect
      ~initial_backoff:(sec 0.01)
      ~on_reconnect:(fun () -> print_endline "on_reconnect")
      config
    >>| ok_exn
  in
  let messages = Ws.messages ws in
  let%bind () = Ws.subscribe ws All_mids in
  (* Subscribing twice sends nothing. *)
  let%bind () = Ws.subscribe ws All_mids in
  let%bind () = read_and_print messages 3 in
  [%expect
    {|
    (Unknown (channel mystery) (data 1))
    (Subscription_response (Subscribe All_mids))
    (All_mids ((BTC 100)))
    |}];
  (* The server drops the connection; the client reconnects and resubscribes. *)
  let%bind () =
    Websocket.close
      (Queue.get server.sockets 0)
      ~code:Endpoint_going_away
      ~reason:"restart"
  in
  let%bind () = read_and_print messages 2 in
  [%expect
    {|
    on_reconnect
    (Subscription_response (Subscribe All_mids))
    (All_mids ((BTC 200)))
    |}];
  let%bind () = Ws.send ws Ping in
  let%bind () = read_and_print messages 1 in
  let%bind () = Ws.close ws in
  let%bind () = read_and_print messages 1 in
  print_s [%sexp (Queue.to_list server.received : (int * string) list)];
  [%expect
    {|
    Pong
    eof
    ((1 "{\"method\":\"subscribe\",\"subscription\":{\"type\":\"allMids\"}}")
     (2 "{\"method\":\"subscribe\",\"subscription\":{\"type\":\"allMids\"}}")
     (2 "{\"method\":\"ping\"}"))
    |}];
  Tcp.Server.close ~close_existing_connections:true server.tcp
;;

let%expect_test "a silent connection is replaced" =
  Log.Global.set_output [];
  let%bind server = Server.start ~pong:false () in
  let config =
    Config.create
      ~base_url:
        (Uri.of_string
           (sprintf "http://127.0.0.1:%d" (Tcp.Server.listening_on server.tcp)))
      ()
  in
  let reconnected = Ivar.create () in
  let%bind ws =
    Ws.connect
      ~ping_interval:(sec 0.05)
      ~initial_backoff:(sec 0.01)
      ~on_reconnect:(Ivar.fill_if_empty reconnected)
      config
    >>| ok_exn
  in
  (* The first connection sends "mystery" and "garbage" frames, so the first ping goes
     out; the second interval sees nothing and closes the socket. *)
  let%bind result = Clock.with_timeout (sec 5.) (Ivar.read reconnected) in
  let%bind () = Ws.close ws in
  print_s
    [%sexp
      ((match result with
        | `Result () -> "reconnected"
        | `Timeout -> "timeout")
       : string)
      , (Queue.length server.sockets : int)
      , (List.filter (Queue.to_list server.received) ~f:(fun (n, _) -> n = 1)
         : (int * string) list)];
  [%expect {| (reconnected 2 ((1 "{\"method\":\"ping\"}"))) |}];
  Tcp.Server.close ~close_existing_connections:true server.tcp
;;
