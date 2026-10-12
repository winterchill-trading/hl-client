(** The subset of MessagePack needed to hash Hyperliquid actions.

    Encodes like Python's [msgpack.packb] with its defaults: the smallest integer form,
    floats as float64, strings as [str] and maps in the given key order. *)

open! Core

type t =
  | Nil
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Map of (string * t) list
[@@deriving sexp_of]

(** [of_yojson json] maps JSON onto MessagePack, keeping object key order. Raises on
    [`Intlit] values that do not fit an [int], and on [`Variant]. *)
val of_yojson : Yojson.Safe.t -> t

val to_string : t -> string
