(** A live {!Market_data.Feed.t} over Hyperliquid's public websocket.

    Read-only: no address or signature is needed, so it works against the simulator and
    against the real Hyperliquid API alike. Per symbol (a Hyperliquid coin such as
    ["BTC"]) it subscribes to

    - [Trades]: the [trades] channel; [exch_ts] is the trade's [time], [trade_id] its
      [tid].
    - [L2]: the [l2Book] channel. Every [l2Book] message is a full book, so each one
      becomes a [Snapshot]; after a reconnect the resubscription brings a fresh snapshot.
    - [Bbo]: the top of the same [l2Book] messages (emitted when both sides are
      non-empty).
    - [Funding]: the [activeAssetCtx] channel (current [funding], [markPx], [oraclePx] as
      the index). The message has no timestamp, so [exch_ts] is the receive time, and
      [next_funding_time] is the next full hour (Hyperliquid funds hourly). The simulator
      does not serve this channel.

    [recv_ts] is [Time_ns.now ()] when the frame is read. Instruments are
    [Instrument.create Hyperliquid symbol].

    The connection is a {!Venue_net.Ws_client}: on disconnect it reconnects with
    exponential backoff and resubscribes. Frames that fail to parse are logged and
    dropped. Pipes of streams that were not requested are closed from the start; the
    others are written without pushback, so read every requested pipe (or close it). *)

open! Core
open! Async

(** [create config ~symbols ~streams] connects to [config.ws_url] and subscribes. Raises
    if the first connection fails.

    [on_raw] sees every data frame of a subscribed stream exactly as received, before it
    is parsed. An [l2Book] frame feeding both [L2] and [Bbo] is passed once per stream.
    Subscription acks, pongs and error frames are not passed. [Private] in [streams] is
    ignored. *)
val create
  :  ?on_raw:(stream:Market_data.Stream.t -> recv_ts:Time_ns.t -> string -> unit)
  -> Config.t
  -> symbols:string list
  -> streams:Market_data.Stream.t list
  -> Market_data.Feed.t Deferred.t

(** Pure pieces, exposed for testing. *)
module For_testing : sig
  (** The JSON subscribe requests for these symbols and streams, in order. *)
  val subscribe_requests
    :  symbols:string list
    -> streams:Market_data.Stream.t list
    -> string list

  (** The ["channel"] of a frame, found without parsing the JSON. *)
  val channel : string -> string option

  (** The requested streams a frame of this channel feeds. *)
  val streams_of_channel
    :  streams:Market_data.Stream.t list
    -> string
    -> Market_data.Stream.t list

  (** Normalized events of one frame for the requested streams. Frames of other channels
      give [Ok []]. *)
  val normalize
    :  streams:Market_data.Stream.t list
    -> recv_ts:Time_ns.t
    -> string
    -> Market_data.Event.t list Or_error.t
end
