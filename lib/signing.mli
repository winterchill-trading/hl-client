(** Hyperliquid request signing for L1 actions (orders, cancels, modifies) and for
    user-signed approval actions (see below).

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

(** Signs a 32-byte digest (RFC 6979 nonces, as the SDK's [eth_account]). *)
val sign_digest : Private_key.t -> string -> Signature.t

(** The address that produced [signature] over [digest]. *)
val recover : digest:string -> Signature.t -> Address.t Or_error.t

(** {2 User-signed actions}

    [approveAgent] and [approveBuilderFee] are signed directly as EIP-712 typed data
    instead of through an L1 action hash (Python SDK [sign_user_signed_action] and
    [user_signed_payload]): domain
    [{name: "HyperliquidSignTransaction", version: "1", chainId: signatureChainId, verifyingContract: 0x0}],
    primary type and fields from {!User_signed_action.Eip712}. The exchange recovers the
    signer from the signature, so these must be signed by the account's own key, never by
    an agent. *)

(** The EIP-712 digest [keccak256(0x1901 || domainSeparator || hashStruct(message))] in
    the [HyperliquidSignTransaction] domain, for any primary type. *)
val user_signed_digest
  :  chain_id:int
  -> primary_type:string
  -> (string * User_signed_action.Eip712.Value.t) list
  -> string

(** [user_signed_digest] for an approval action. *)
val user_action_digest : User_signed_action.t -> string

val sign_user_action : Private_key.t -> User_signed_action.t -> Signature.t

(** The address that signed [action]; compare it with the account before posting. *)
val recover_user_action : User_signed_action.t -> Signature.t -> Address.t Or_error.t

(** A [?sign] hook for {!Exchange}: reads [action], [nonce] and [vaultAddress] from the
    unsigned request body and returns the signature JSON. *)
val sign_request
  :  Private_key.t
  -> is_mainnet:bool
  -> Yojson.Safe.t
  -> Yojson.Safe.t option
