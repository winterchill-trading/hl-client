(** Zero-allocation readers for the high-rate Hyperliquid websocket frames ([trades] and
    [l2Book]). They read the fields {!Normalizer} needs straight from the frame into
    reused mutable records, without building a JSON tree. Every reader returns [false] (or
    [-1] for a position) on any shape it does not recognise; the caller then falls back to
    the Yojson decoder, so the scanner only has to be right on the frames it accepts.

    Decimals are read as {!Fixed_decimal} units (10^-8) with the same rules as
    [Fixed_decimal.of_string] (extra fraction digits truncated); signs, exponents and
    values out of range are left to the fallback. Strings with escapes are left to the
    fallback too. *)

open! Core

(** The channel of a frame. *)
module Channel : sig
  type t =
    | Trades
    | L2_book
    | Active_asset_ctx
    | Other
  [@@deriving sexp_of]
end

(** The channel of a frame, from the first ["channel"] key. [Other] when there is none. *)
val channel : string -> Channel.t
[@@zero_alloc]

(** Position just after the key [key] (which includes its quotes, e.g. [{|"px"|}]), the
    colon and any whitespace, searching from [from] up to [stop] (exclusive); [-1] when
    absent. *)
val find_key : string -> from:int -> stop:int -> string -> int
[@@zero_alloc]

(** [value_end s pos] where [s.[pos]] is ['{'] or ['[']: the position just after the
    matching close, skipping strings; [-1] when unterminated. *)
val value_end : string -> int -> int
[@@zero_alloc]

(** The quoted decimal at [pos] in {!Fixed_decimal} units, or [-1]. *)
val decimal : string -> int -> int
[@@zero_alloc]

(** The non-negative bare integer at [pos], or [-1]. *)
val int_at : string -> int -> int
[@@zero_alloc]

(** Scratch for reading an object's key/value pairs in one pass. *)
module Pair : sig
  type t
end

module Trade : sig
  (** One element of a [trades] frame's [data] array. *)
  type t =
    { mutable coin_pos : int (** first byte of the coin name in the frame *)
    ; mutable coin_len : int
    ; mutable buy : bool
    ; mutable px : int (** units of 10^-8 *)
    ; mutable sz : int (** units of 10^-8 *)
    ; mutable time : int (** milliseconds since the epoch *)
    ; mutable tid : int
    ; pair : Pair.t
    }

  val create : unit -> t

  (** Reads the trade object opening at [pos] (its ['{']) in one pass: the position just
      after its ['}'], or [-1]. *)
  val read_at : string -> int -> t -> int
  [@@zero_alloc]
end

(** Position of the [data] array of a [trades] frame (its ['[']), or [-1]. *)
val trades_data : string -> int
[@@zero_alloc]

(** [first_element s pos] with [s.[pos]] an opening ['\['], and [next_element s pos]
    just after an element: the position of the next element, [-1] at the closing [']'],
    [-2] on anything else. Whitespace is skipped. *)
val first_element : string -> int -> int
[@@zero_alloc]

val next_element : string -> int -> int [@@zero_alloc]

module Book : sig
  (** An [l2Book] frame, levels best first. Holds at most [capacity] levels a side; a
      deeper book is left to the fallback. *)
  type t =
    { mutable coin_pos : int
    ; mutable coin_len : int
    ; mutable time : int
    ; mutable n_bids : int
    ; mutable n_asks : int
    ; mutable side_end : int (** scratch: just after the last side array read *)
    ; bid_px : int array
    ; bid_sz : int array
    ; ask_px : int array
    ; ask_sz : int array
    ; pair : Pair.t
    }

  val capacity : int
  val create : unit -> t
  (** Reads the first [depth] (at most [capacity]) levels of each side and steps over
      the rest without reading them. *)
  val read : depth:int -> string -> t -> bool
  [@@zero_alloc]
end

(** [equal_sub s ~pos ~len name]: the bytes of [s] at [pos] are exactly [name]. *)
val equal_sub : string -> pos:int -> len:int -> string -> bool
[@@zero_alloc]
