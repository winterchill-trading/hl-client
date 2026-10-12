(** Keccak-256 as used by Ethereum (the original Keccak padding, not NIST SHA3-256). *)

open! Core

(** [digest s] is the 32-byte Keccak-256 hash of [s]. *)
val digest : string -> string

(** [digest_hex s] is [digest s] as 64 lowercase hex digits. *)
val digest_hex : string -> string
