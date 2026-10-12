(* A Hyperliquid-like websocket server on 127.0.0.1 for tests, built from the same
   async_websocket library with [~role:Server]. *)

open! Core
open Async
open Hl_protocol

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

let all_mids ~connection _subscription =
  [ sprintf {|{"channel":"allMids","data":{"mids":{"BTC":"%d00"}}}|} connection ]
;;

let reply ~pong ~on_subscribe to_client n frame =
  match Yojson.Safe.from_string frame |> Ws_request.t_of_yojson with
  | Subscribe subscription ->
    Pipe.write_without_pushback
      to_client
      (sprintf {|{"channel":"subscriptionResponse","data":%s}|} frame);
    List.iter
      (on_subscribe ~connection:n subscription)
      ~f:(Pipe.write_without_pushback to_client)
  | Ping -> if pong then Pipe.write_without_pushback to_client {|{"channel":"pong"}|}
  | Unsubscribe _ -> ()
;;

let handle ~pong ~greeting ~on_subscribe ~received ~sockets reader writer =
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
  if n = 1 then List.iter greeting ~f:(Pipe.write_without_pushback to_client);
  Pipe.iter_without_pushback from_client ~f:(fun frame ->
    Queue.enqueue received (n, frame);
    reply ~pong ~on_subscribe to_client n frame)
;;

let default_greeting = [ {|{"channel":"mystery","data":1}|}; "garbage" ]

let start ?(pong = true) ?(greeting = default_greeting) ?(on_subscribe = all_mids) () =
  let received = Queue.create () in
  let sockets = Queue.create () in
  let%map tcp =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      Tcp.Where_to_listen.of_port_chosen_by_os
      (fun _ reader writer ->
         handle ~pong ~greeting ~on_subscribe ~received ~sockets reader writer)
  in
  { tcp; received; sockets }
;;

let base_url t =
  Uri.of_string (sprintf "http://127.0.0.1:%d" (Tcp.Server.listening_on t.tcp))
;;

let drop t n =
  Websocket.close (Queue.get t.sockets n) ~code:Endpoint_going_away ~reason:"restart"
;;

let close t = Tcp.Server.close ~close_existing_connections:true t.tcp
