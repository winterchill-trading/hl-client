(** Time and minor words per frame for websocket frames read by [Live_feed], against a
    {!Blast_server} writing one recorded-shaped [trades] frame (one BTC trade) over and
    over. *)

open! Core
open Async

type result =
  { seconds : float (** wall time from the first write to the last frame consumed *)
  ; words : float (** minor words per frame over the same window *)
  }

(** One BTC trade on the [trades] channel. *)
val frame : string

(** [floor server ~n ~runs] reads frames with a bare [Venue_net.Ws_client] whose
    [on_message] only counts them: socket reads and websocket framing, which every feed
    pays. One warm-up run, then [runs] results. *)
val floor : Blast_server.t -> n:int -> runs:int -> result list Deferred.t

(** [live_feed server ~n ~runs] reads them through [Live_feed] with the [Trades] stream,
    consuming the trades pipe a batch at a time. *)
val live_feed : Blast_server.t -> n:int -> runs:int -> result list Deferred.t
