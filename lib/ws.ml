open! Core
open Async
open Hl_protocol

let base64 s =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let n = String.length s in
  let buf = Buffer.create ((n + 2) / 3 * 4) in
  let byte i = if i < n then Char.to_int s.[i] else 0 in
  let i = ref 0 in
  while !i < n do
    let b = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
    let sextet shift = alphabet.[(b lsr shift) land 63] in
    Buffer.add_char buf (sextet 18);
    Buffer.add_char buf (sextet 12);
    Buffer.add_char buf (if !i + 1 < n then sextet 6 else '=');
    Buffer.add_char buf (if !i + 2 < n then sextet 0 else '=');
    i := !i + 3
  done;
  Buffer.contents buf
;;

let default_port uri =
  match Uri.port uri, Uri.scheme uri with
  | Some port, _ -> port
  | None, Some ("wss" | "https") -> 443
  | None, _ -> 80
;;

let is_tls uri =
  match Uri.scheme uri with
  | Some ("wss" | "https") -> true
  | _ -> false
;;

let handshake_request uri ~key =
  let host = Option.value (Uri.host uri) ~default:"localhost" in
  let host =
    match Uri.port uri with
    | None -> host
    | Some port -> sprintf "%s:%d" host port
  in
  let path =
    match Uri.path_and_query uri with
    | "" -> "/"
    | path -> path
  in
  String.concat
    [ sprintf "GET %s HTTP/1.1\r\n" path
    ; sprintf "Host: %s\r\n" host
    ; "Upgrade: websocket\r\n"
    ; "Connection: Upgrade\r\n"
    ; sprintf "Sec-WebSocket-Key: %s\r\n" key
    ; "Sec-WebSocket-Version: 13\r\n"
    ; "\r\n"
    ]
;;

let check_handshake_response ~key ~status_line ~headers =
  let header name =
    List.find_map headers ~f:(fun (k, v) ->
      Option.some_if (String.Caseless.equal k name) (String.strip v))
  in
  match String.split status_line ~on:' ' with
  | _ :: "101" :: _ ->
    let expected = Websocket.sec_websocket_accept_header_value ~sec_websocket_key:key in
    (match header "Sec-WebSocket-Accept" with
     | Some accept when String.equal accept expected -> Ok ()
     | accept ->
       Or_error.error_s
         [%message
           "bad Sec-WebSocket-Accept" (accept : string option) (expected : string)])
  | _ -> Or_error.error_s [%message "websocket upgrade refused" (status_line : string)]
;;

let decode frame =
  Or_error.try_with (fun () -> Ws_message.t_of_yojson (Yojson.Safe.from_string frame))
;;

let encode request = Yojson.Safe.to_string (Ws_request.yojson_of_t request)
let next_backoff ~max span = Time_float.Span.min max (Time_float.Span.scale span 2.)

let backoff ~initial ~max ~attempts =
  List.folding_map (List.range 0 attempts) ~init:initial ~f:(fun span _ ->
    next_backoff ~max span, span)
;;

(* The key only has to be unpredictable enough to defeat caching proxies. Seeded from the
   clock because Base forbids [Random.State.make_self_init] inside inline tests. *)
let random_state =
  lazy (Random.State.make [| Time_ns.to_int_ns_since_epoch (Time_ns.now ()) |])
;;

let new_key () =
  let state = force random_state in
  base64 (String.init 16 ~f:(fun _ -> Char.of_int_exn (Random.State.int state 256)))
;;

(* Reads the status line and headers of the upgrade response. Bytes after the blank line
   stay buffered in [reader] for the websocket framing layer. *)
let read_handshake_response reader =
  let read_line () =
    match%map Reader.read_line reader with
    | `Ok line -> Ok line
    | `Eof -> Or_error.error_string "connection closed during websocket handshake"
  in
  let rec read_headers acc =
    match%bind read_line () with
    | Error _ as e -> return e
    | Ok "" -> return (Ok (List.rev acc))
    | Ok line ->
      (match String.lsplit2 line ~on:':' with
       | Some (k, v) -> read_headers ((k, v) :: acc)
       | None -> read_headers acc)
  in
  match%bind read_line () with
  | Error _ as e -> return e
  | Ok status_line ->
    let%map headers = read_headers [] in
    Or_error.map headers ~f:(fun headers -> status_line, headers)
;;

let tcp_connect uri =
  let host = Option.value (Uri.host uri) ~default:"localhost" in
  let uri = Uri.with_port uri (Some (default_port uri)) in
  let%bind addr = Conduit_async.V3.resolve_uri uri in
  let addr : _ Conduit_async.V3.addr =
    match addr with
    | (OpenSSL (inet, _) | Inet inet) when is_tls uri ->
      (* [resolve_uri] builds a TLS config without SNI, which CDNs reject. *)
      OpenSSL (inet, Conduit_async.V3.Ssl.Config.create ~hostname:host ())
    | addr -> addr
  in
  let%map _socket, reader, writer = Conduit_async.V3.connect addr in
  reader, writer
;;

module Connection = struct
  type t =
    { ws : string Websocket.t
    ; from_server : string Pipe.Reader.t
    ; to_server : string Pipe.Writer.t
    }

  let open_ uri =
    Monitor.try_with_or_error ~extract_exn:true (fun () ->
      let%bind reader, writer = tcp_connect uri in
      let key = new_key () in
      Writer.write writer (handshake_request uri ~key);
      let%bind () = Writer.flushed writer in
      match%bind read_handshake_response reader with
      | Ok (status_line, headers) ->
        (match check_handshake_response ~key ~status_line ~headers with
         | Ok () ->
           let ws = Websocket.create ~opcode:`Text ~role:Client reader writer in
           let from_server, to_server = Websocket.pipes ws in
           return { ws; from_server; to_server }
         | Error err ->
           let%bind () = Writer.close writer in
           let%bind () = Reader.close reader in
           Error.raise err)
      | Error err ->
        let%bind () = Writer.close writer in
        let%bind () = Reader.close reader in
        Error.raise err)
    |> Deferred.Or_error.tag_s
         ~tag:[%message "websocket connect" ~uri:(Uri.to_string uri : string)]
  ;;

  let close t = Websocket.close t.ws ~code:Normal_closure ~reason:""
  let send t frame = Pipe.write_if_open t.to_server frame
end

type t =
  { config : Config.t
  ; ping_interval : Time_float.Span.t
  ; initial_backoff : Time_float.Span.t
  ; max_backoff : Time_float.Span.t
  ; on_reconnect : unit -> unit
  ; mutable subscriptions : Subscription.Set.t
  ; mutable connection : Connection.t option
  ; messages_reader : Ws_message.t Pipe.Reader.t
  ; messages_writer : Ws_message.t Pipe.Writer.t
  ; closed : unit Ivar.t
  }

let messages t = t.messages_reader
let subscriptions t = Set.to_list t.subscriptions
let is_connected t = Option.is_some t.connection
let is_closed t = Ivar.is_full t.closed

let send t request =
  match t.connection with
  | None -> return ()
  | Some connection -> Connection.send connection (encode request)
;;

let subscribe t subscription =
  if Set.mem t.subscriptions subscription
  then return ()
  else (
    t.subscriptions <- Set.add t.subscriptions subscription;
    send t (Subscribe subscription))
;;

let unsubscribe t subscription =
  if not (Set.mem t.subscriptions subscription)
  then return ()
  else (
    t.subscriptions <- Set.remove t.subscriptions subscription;
    send t (Unsubscribe subscription))
;;

let handle_frame t frame =
  match decode frame with
  | Ok message ->
    (match message with
     | Unknown { channel; _ } ->
       Log.Global.debug_s [%message "hl-client ws: unknown channel" (channel : string)]
     | _ -> ());
    Pipe.write_without_pushback_if_open t.messages_writer message
  | Error error ->
    Log.Global.debug_s
      [%message "hl-client ws: undecodable frame" (frame : string) (error : Error.t)]
;;

(* Runs one connection until it closes. *)
let serve t (connection : Connection.t) =
  t.connection <- Some connection;
  List.iter (subscriptions t) ~f:(fun subscription ->
    Pipe.write_without_pushback_if_open
      connection.to_server
      (encode (Subscribe subscription)));
  let received_since_last_ping = ref true in
  let finished = Pipe.closed connection.from_server in
  Clock.every' ~start:(after t.ping_interval) ~stop:finished t.ping_interval (fun () ->
    if !received_since_last_ping
    then (
      received_since_last_ping := false;
      Connection.send connection (encode Ping))
    else (
      Log.Global.info_s [%message "hl-client ws: no data received, reconnecting"];
      Connection.close connection));
  let%bind () =
    Pipe.iter_without_pushback connection.from_server ~f:(fun frame ->
      received_since_last_ping := true;
      handle_frame t frame)
  in
  t.connection <- None;
  let%map reason = Websocket.close_finished connection.ws in
  Log.Global.info_s
    [%message
      "hl-client ws: disconnected"
        ~url:(Uri.to_string t.config.ws_url : string)
        (reason : Websocket.Connection_close_reason.t * string * Core.Info.t option)]
;;

let rec reconnect_loop t ~delay =
  if is_closed t
  then return ()
  else (
    let%bind () = Deferred.any [ after delay; Ivar.read t.closed ] in
    if is_closed t
    then return ()
    else (
      match%bind Connection.open_ t.config.ws_url with
      | Error error ->
        Log.Global.error_s [%message "hl-client ws: reconnect failed" (error : Error.t)];
        reconnect_loop t ~delay:(next_backoff ~max:t.max_backoff delay)
      | Ok connection ->
        if is_closed t
        then Connection.close connection
        else (
          Log.Global.info_s
            [%message
              "hl-client ws: reconnected" ~url:(Uri.to_string t.config.ws_url : string)];
          (* [serve] enqueues the subscriptions before its first bind. *)
          let served = serve t connection in
          t.on_reconnect ();
          let%bind () = served in
          reconnect_loop t ~delay:t.initial_backoff)))
;;

let connect
  ?(ping_interval = Time_float.Span.of_sec 50.)
  ?(initial_backoff = Time_float.Span.of_sec 0.5)
  ?(max_backoff = Time_float.Span.of_sec 30.)
  ?(on_reconnect = fun () -> ())
  (config : Config.t)
  =
  let%map.Deferred.Or_error connection = Connection.open_ config.ws_url in
  let messages_reader, messages_writer = Pipe.create () in
  let t =
    { config
    ; ping_interval
    ; initial_backoff
    ; max_backoff
    ; on_reconnect
    ; subscriptions = Subscription.Set.empty
    ; connection = None
    ; messages_reader
    ; messages_writer
    ; closed = Ivar.create ()
    }
  in
  don't_wait_for
    (let%bind () = serve t connection in
     reconnect_loop t ~delay:initial_backoff);
  t
;;

let close t =
  if is_closed t
  then return ()
  else (
    Ivar.fill_exn t.closed ();
    Pipe.close t.messages_writer;
    match t.connection with
    | None -> return ()
    | Some connection -> Connection.close connection)
;;

module For_testing = struct
  let base64 = base64
  let handshake_request = handshake_request
  let check_handshake_response = check_handshake_response
  let decode = decode
  let encode = encode
  let backoff = backoff
end
