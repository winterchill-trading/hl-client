(** Hyperliquid request signing for L1 actions (orders, cancels, modifies).

    Follows the official Python SDK ([hyperliquid/utils/signing.py]):

    + [action_hash] is Keccak-256 over the MessagePack encoding of the action JSON, the
      nonce as 8 big-endian bytes, a vault marker ([0x00], or [0x01] followed by the 20
      address bytes) and, when set, [0x00] plus [expiresAfter] as 8 bytes.
    + The hash becomes the [connectionId] of a "phantom agent"
      [{source: "a" (mainnet) | "b" (testnet), connectionId}].
    + The agent is signed as EIP-712 typed data in the domain
      [{name: "Exchange", version: "1", chainId: 1337, verifyingContract: 0x0}] with
      secp256k1, giving [{r, s, v}] with [v] in [{27, 28}].

    Key order in the action JSON matters, since it is hashed as given. The encoders in
    [Hl_protocol] emit keys in the order the exchange expects. *)

open! Core
open Hl_protocol

module Private_key : sig
  (** A secp256k1 secret key. Its sexp never shows the key. *)
  type t [@@deriving sexp_of]

  (** 64 hex digits, with or without a [0x] prefix. *)
  val of_hex : string -> t Or_error.t

  (** Reads [var] (default [HL_PRIVATE_KEY]) from the environment. *)
  val of_env : ?var:string -> unit -> t Or_error.t

  (** The Ethereum address of the key, which is the account the exchange sees. *)
  val address : t -> Address.t
end

module Signature : sig
  (** [r] and [s] are [0x]-prefixed hex without leading zeros, as the SDK sends them. *)
  type t =
    { r : string
    ; s : string
    ; v : int
    }
  [@@deriving sexp, equal, yojson]
end

(** The 32-byte [connectionId] for an action. *)
val action_hash
  :  ?expires_after:int
  -> vault_address:Address.t option
  -> nonce:int
  -> Yojson.Safe.t
  -> string

(** The 32-byte EIP-712 digest of the phantom agent for [connection_id]. *)
val l1_digest : is_mainnet:bool -> connection_id:string -> string

val sign_l1_action
  :  ?expires_after:int
  -> Private_key.t
  -> is_mainnet:bool
  -> vault_address:Address.t option
  -> nonce:int
  -> Yojson.Safe.t
  -> Signature.t

(** The address that produced [signature] over [digest]. *)
val recover : digest:string -> Signature.t -> Address.t Or_error.t

(** A [?sign] hook for {!Exchange}: reads [action], [nonce] and [vaultAddress] from the
    unsigned request body and returns the signature JSON. *)
val sign_request
  :  Private_key.t
  -> is_mainnet:bool
  -> Yojson.Safe.t
  -> Yojson.Safe.t option
