(** Fills and positions of other accounts (e.g. leaders to copy), over one shared
    websocket connection.

    Any address can be subscribed to [userFills]; no key is needed. Hyperliquid allows at
    most 10 distinct user addresses per connection and per IP (see the copy-trading design
    in the specs repo), so a [t] holds one connection and one [userFills] subscription per
    address, shared by every consumer of that address: a hundred followers of one leader
    cost one subscription.

    Per address the feed

    - drops fills it has already delivered (by [tid]): the server resends the snapshot
      after every resubscription, including after a reconnect;
    - after each message with new fills, emits the resulting position of every coin
      touched, [startPosition] plus the signed size of the coin's latest fill;
    - on reconnect, emits [Resync] before the replayed snapshot, whose new fills (those
      missed while disconnected) come with [is_snapshot = true];
    - gives a consumer that subscribes later the positions known so far, then the live
      updates.

    [orderUpdates] frames carry no user address, so they cannot be shared on one
    connection and are not part of this feed. *)

open! Core
open Async
open Hl_protocol

module Update : sig
  type t =
    | Fills of
        { user : Address.t
        ; is_snapshot : bool
        ; fills : User_fill.t list (** new fills only, in message order *)
        }
    | Position of
        { user : Address.t
        ; coin : Coin.t
        ; szi : Size.t (** signed: positive long, negative short *)
        ; time : int (** of the fill that produced it, epoch ms *)
        }
    | Resync of { user : Address.t }
    (** The connection was replaced; fills missed meanwhile follow as a snapshot. *)
  [@@deriving sexp_of]
end

type t

(** The documented per-connection limit, 10. *)
val default_max_users : int

(** Opens the connection ([Config.ws_url]); [ws_args] are passed to {!Ws.connect}. *)
val create
  :  ?max_users:int
  -> ?ping_interval:Time_float.Span.t
  -> ?initial_backoff:Time_float.Span.t
  -> ?max_backoff:Time_float.Span.t
  -> Config.t
  -> t Or_error.t Deferred.t

(** A new consumer of [address]'s updates. Subscribes on the connection for the first
    consumer of an address; fails when [max_users] distinct addresses are already
    subscribed. Close the reader to stop; the last consumer of an address to close it
    unsubscribes. Updates are written without pushback, so keep reading. *)
val subscribe : t -> address:Address.t -> Update.t Pipe.Reader.t Or_error.t Deferred.t

(** Addresses with at least one consumer. *)
val users : t -> Address.t list

(** Consumers of [address]. *)
val consumers : t -> address:Address.t -> int

(** Closes the connection and every consumer pipe. *)
val close : t -> unit Deferred.t
