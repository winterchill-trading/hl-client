(** Hyperliquid websocket frames to normalized {!Market_data} events. Pure; the Async
    driver is {!Live_feed}, which runs exactly this code on every frame.

    - [trades]: one {!Market_data.Trade.t} per print; [exch_ts] is the trade's [time],
      [trade_id] its [tid].
    - [l2Book]: every message is a full book, so each one becomes an
      {!Market_data.L2_update.t} [Snapshot], followed by a {!Market_data.Bbo.t} when both
      sides are non-empty and the top of book (prices and sizes) changed since the last
      one of that instrument.
    - [activeAssetCtx]: a {!Market_data.Funding.t} (current [funding], [markPx],
      [oraclePx] as the index). The message has no timestamp, so [exch_ts] is [recv_ts],
      and [next_funding_time] is the next full hour (Hyperliquid funds hourly). Spot
      contexts have no funding and give nothing.

    Instruments are [Instrument.create Hyperliquid coin], created once per coin and shared
    by every event of that coin. Frames of other channels (subscription acks, pongs,
    errors) give [Ok []].

    [trades] and [l2Book] frames are read by {!Frame_scan} without a JSON tree; frames it
    refuses, and [activeAssetCtx] frames, are decoded with Yojson. Both paths give the
    same events. *)

open! Core

type t

(** [streams] (default [Trades], [Bbo], [L2] and [Funding]) are the streams events are
    produced for; an [l2Book] frame gives no [L2] event without [L2], no [Bbo] without
    [Bbo]. *)
val create : ?streams:Market_data.Stream.t list -> unit -> t

val instrument : string -> Market_data.Instrument.t

(** The events of one frame, in order. [Error] when a frame of a wanted channel does not
    parse. *)
val frame : t -> recv_ts:Time_ns.t -> string -> Market_data.Event.t list Or_error.t

(** Forgets the last top of book, so the next [l2Book] frame emits a bbo again (after a
    reconnect). *)
val reset : t -> unit

(** The ["channel"] of a frame, found without parsing the JSON. *)
val channel : string -> string option

(** The streams among [streams] a frame of this channel feeds. *)
val streams_of_channel
  :  streams:Market_data.Stream.t list
  -> string
  -> Market_data.Stream.t list

module For_testing : sig
  (** [frame] through the Yojson decoder only, never the {!Frame_scan} readers: the
      reference the scanner path is tested against. *)
  val frame_yojson
    :  t
    -> recv_ts:Time_ns.t
    -> string
    -> Market_data.Event.t list Or_error.t
end
