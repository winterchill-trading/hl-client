(** Websocket client for Hyperliquid's [/ws] endpoint.

    A [t] owns one logical connection. When the socket closes (or goes quiet), it
    reconnects with exponential backoff and replays every active subscription; the server
    then sends fresh snapshots. Messages from all connections are delivered, in order, on
    a single pipe.

    Both [ws://] (simulator) and [wss://] (real Hyperliquid, via OpenSSL with SNI) URLs
    are supported. The transport (handshake, keepalive, watchdog, reconnect) is
    [Venue_net.Ws_client]; this module adds the typed messages and subscription
    bookkeeping. *)

open! Core
open Async
open Hl_protocol

type t

(** [connect config] opens the first connection to [config.ws_url] and returns once the
    handshake has succeeded. Later disconnects are retried in the background.

    - [ping_interval] (default 50s): send [{"method": "ping"}] this often. Hyperliquid
      drops connections that are silent for 60s. If nothing at all was received during two
      consecutive intervals, the connection is considered dead and is replaced.
    - [initial_backoff] (default 0.5s) and [max_backoff] (default 30s) bound the delay
      between reconnection attempts; the delay doubles after each failure and resets after
      a successful handshake. [backoff_jitter] (default 0.2) spreads each delay over
      [delay * (1 +/- backoff_jitter)].
    - [time_source] (default the wall clock) drives the keepalive, the silence watchdog,
      the handshake timeout and the reconnection delays, so tests can step them with a
      synthetic clock.
    - [on_reconnect] is called after each successful reconnection (not the first
      connection), once subscriptions have been resent. Use it to resynchronise state that
      may have changed while disconnected, e.g. open orders. *)
val connect
  :  ?ping_interval:Time_float.Span.t
  -> ?initial_backoff:Time_float.Span.t
  -> ?max_backoff:Time_float.Span.t
  -> ?backoff_jitter:float
  -> ?time_source:Time_source.t
  -> ?on_reconnect:(unit -> unit)
  -> Config.t
  -> t Or_error.t Deferred.t

(** Every decoded server message, including [Subscription_response], [Pong], [Error] and
    [Unknown] channels. Frames that fail to decode are logged at debug level and dropped.
    The pipe is closed by [close]. Consumers should keep up: messages are buffered without
    pushback. *)
val messages : t -> Ws_message.t Pipe.Reader.t

(** Subscribe now (if connected) and after every reconnection. Subscribing twice is a
    no-op. *)
val subscribe : t -> Subscription.t -> unit Deferred.t

val unsubscribe : t -> Subscription.t -> unit Deferred.t
val subscriptions : t -> Subscription.t list
val is_connected : t -> bool

(** Successful reconnections so far (the first connection does not count). *)
val reconnects : t -> int

(** Close the socket, stop reconnecting and close [messages]. *)
val close : t -> unit Deferred.t

(** Send an arbitrary request on the current connection; dropped if disconnected. *)
val send : t -> Ws_request.t -> unit Deferred.t

(** Pure pieces, exposed for testing. The handshake, backoff and keepalive live in
    [Venue_net.Ws_client] and are tested there. *)
module For_testing : sig
  val decode : string -> Ws_message.t Or_error.t
  val encode : Ws_request.t -> string
end
