open! Core
open Async
open Hl_protocol
module Ws_client = Venue_net.Ws_client

let decode frame =
  Or_error.try_with (fun () -> Ws_message.t_of_yojson (Yojson.Safe.from_string frame))
;;

let encode request = Yojson.Safe.to_string (Ws_request.yojson_of_t request)

type t =
  { client : Ws_client.t
  ; subscriptions : Subscription.Set.t ref
  ; messages : Ws_message.t Pipe.Reader.t
  }

let messages t = t.messages
let subscriptions t = Set.to_list !(t.subscriptions)
let is_connected t = Ws_client.is_connected t.client
let reconnects t = Ws_client.reconnects t.client
let send t request = Ws_client.send t.client (encode request)

let subscribe t subscription =
  if Set.mem !(t.subscriptions) subscription
  then return ()
  else (
    t.subscriptions := Set.add !(t.subscriptions) subscription;
    send t (Subscribe subscription))
;;

let unsubscribe t subscription =
  if not (Set.mem !(t.subscriptions) subscription)
  then return ()
  else (
    t.subscriptions := Set.remove !(t.subscriptions) subscription;
    send t (Unsubscribe subscription))
;;

let decode_frame frame =
  match decode frame with
  | Ok message ->
    (match message with
     | Unknown { channel; _ } ->
       Log.Global.debug_s [%message "hl-client ws: unknown channel" (channel : string)]
     | _ -> ());
    Some message
  | Error error ->
    Log.Global.debug_s
      [%message "hl-client ws: undecodable frame" (frame : string) (error : Error.t)];
    None
;;

let span = Time_ns.Span.of_span_float_round_nearest

let connect
  ?(ping_interval = Time_float.Span.of_sec 50.)
  ?(initial_backoff = Time_float.Span.of_sec 0.5)
  ?(max_backoff = Time_float.Span.of_sec 30.)
  ?backoff_jitter
  ?time_source
  ?(on_reconnect = fun () -> ())
  (config : Config.t)
  =
  let subscriptions = ref Subscription.Set.empty in
  (* Resubscriptions are enqueued synchronously, so they go out before anything the caller
     sends from [on_reconnect]. *)
  let on_connect client =
    let resubscribed =
      Deferred.all_unit
        (List.map (Set.to_list !subscriptions) ~f:(fun subscription ->
           Ws_client.send client (encode (Subscribe subscription))))
    in
    if Ws_client.reconnects client > 0 then on_reconnect ();
    resubscribed
  in
  let%map.Deferred.Or_error client =
    Ws_client.connect
      ~ping:(`Text (encode Ping, span ping_interval))
      ~max_silent_intervals:1
      ~initial_backoff:(span initial_backoff)
      ~max_backoff:(span max_backoff)
      ?backoff_jitter
      ?time_source
      ~on_connect
      ~log_name:"hl-client ws"
      ~url:config.ws_url
      ()
  in
  { client
  ; subscriptions
  ; messages = Pipe.filter_map (Ws_client.messages client) ~f:decode_frame
  }
;;

let close t = Ws_client.close t.client

module For_testing = struct
  let decode = decode
  let encode = encode
end
