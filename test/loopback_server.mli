(** A Hyperliquid-like websocket server on 127.0.0.1 for tests.

    It answers the upgrade handshake on any path, acknowledges every subscribe with a
    [subscriptionResponse] followed by the frames of [on_subscribe], and answers pings
    with pongs unless [pong] is false. Connections are numbered from 1. *)

open! Core
open Async
open Hl_protocol

type t =
  { tcp : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; received : (int * string) Queue.t (** (connection number, frame) *)
  ; sockets : string Websocket.t Queue.t
  }

(** [greeting] (default an unknown ["mystery"] channel frame and an unparsable
    ["garbage"]) is sent once, on the first connection. [on_subscribe] defaults to one
    [allMids] frame whose BTC mid is [100 * connection]. *)
val start
  :  ?pong:bool
  -> ?greeting:string list
  -> ?on_subscribe:(connection:int -> Subscription.t -> string list)
  -> unit
  -> t Deferred.t

(** [http://127.0.0.1:<port>]; [Config.create ~base_url] derives the websocket URL. *)
val base_url : t -> Uri.t

(** Close the [n]th (0-based) server-side socket, as a server restart would. *)
val drop : t -> int -> unit Deferred.t

val close : t -> unit Deferred.t
