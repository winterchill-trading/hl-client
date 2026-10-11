(* End-to-end test of [Ws] against a websocket server on 127.0.0.1, built from the same
   async_websocket library with [~role:Server]. *)

open! Core
open Async
open Hl_protocol
open Hl_client

module Server = struct
  type t =
    { tcp : (Socket.Address.Inet.t, int) Tcp.Server.t
    ; received : (int * string) Queue.t (** (connection number, frame) *)
    ; sockets : string Websocket.t Queue.t
    }

  let rec read_headers reader acc =
    match%bind Reader.read_line reader with
    | `Eof | `Ok "" -> return acc
    | `Ok line ->
      read_headers
        reader
        (match String.lsplit2 line ~on:':' with
         | Some (k, v) -> (String.lowercase k, String.strip v) :: acc
         | None -> acc)
  ;;

  let reply ~pong to_client n frame =
    match Yojson.Safe.from_string frame |> Ws_request.t_of_yojson with
    | Subscribe _ ->
      Pipe.write_without_pushback
        to_client
        (sprintf {|{"channel":"subscriptionResponse","data":%s}|} frame);
      Pipe.write_without_pushback
        to_client
        (sprintf {|{"channel":"allMids","data":{"mids":{"BTC":"%d00"}}}|} n)
    | Ping -> if pong then Pipe.write_without_pushback to_client {|{"channel":"pong"}|}
    | Unsubscribe _ -> ()
  ;;

  let handle ~pong ~received ~sockets reader writer =
    let%bind headers = read_headers reader [] in
    let key = List.Assoc.find_exn headers ~equal:String.equal "sec-websocket-key" in
    Writer.write
      writer
      (sprintf
         "HTTP/1.1 101 Switching Protocols\r\n\
          Upgrade: websocket\r\n\
          Connection: Upgrade\r\n\
          Sec-WebSocket-Accept: %s\r\n\
          \r\n"
         (Websocket.sec_websocket_accept_header_value ~sec_websocket_key:key));
    let ws = Websocket.create ~opcode:`Text ~role:Server reader writer in
    Queue.enqueue sockets ws;
    let n = Queue.length sockets in
    let from_client, to_client = Websocket.pipes ws in
    if n = 1
    then (
      Pipe.write_without_pushback to_client {|{"channel":"mystery","data":1}|};
      Pipe.write_without_pushback to_client "garbage");
    Pipe.iter_without_pushback from_client ~f:(fun frame ->
      Queue.enqueue received (n, frame);
      reply ~pong to_client n frame)
  ;;

  let start ?(pong = true) () =
    let received = Queue.create () in
    let sockets = Queue.create () in
    let%map tcp =
      Tcp.Server.create
        ~on_handler_error:`Ignore
        Tcp.Where_to_listen.of_port_chosen_by_os
        (fun _ reader writer -> handle ~pong ~received ~sockets reader writer)
    in
    { tcp; received; sockets }
  ;;
end

let read_and_print messages n =
  Deferred.repeat_until_finished n (fun n ->
    if n = 0
    then return (`Finished ())
    else (
      match%map Clock.with_timeout (sec 5.) (Pipe.read messages) with
      | `Timeout ->
        print_endline "timeout";
        `Finished ()
      | `Result `Eof ->
        print_endline "eof";
        `Finished ()
      | `Result (`Ok message) ->
        print_s [%sexp (message : Ws_message.t)];
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
