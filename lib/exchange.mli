(** The [POST /exchange] endpoint.

    Requests carry an optional opaque [signature] produced by the [?sign] hook;
    {!Signing.sign_request} is the hook for the real exchange, and {!Client} passes it for
    you. The simulator accepts unsigned requests; the real Hyperliquid API rejects them. *)

open! Core
open Async
open Hl_protocol

(** Produces the [signature] JSON from the unsigned request body
    ([{"action": ..., "nonce": ...}] plus [vaultAddress] when set). Returning [None] sends
    the request unsigned. *)
type sign = Yojson.Safe.t -> Yojson.Safe.t option

(** Current time in milliseconds, strictly increasing across calls in this process so that
    two requests in the same millisecond still get distinct nonces. *)
val next_nonce : unit -> int

val build_request
  :  ?sign:sign
  -> ?vault_address:Address.t
  -> nonce:int
  -> Exchange_action.t
  -> Exchange_request.t

(** Decode a response body. Bodies that decode as an exchange response are accepted with
    any HTTP status, since [{"status": "err", ...}] may come with a 4xx. *)
val decode : Http.Response.t -> Exchange_response.t Or_error.t

(** Extra HTTP headers sent with every request: [X-Address: <Config.address>] when the
    config has an address. The simulator takes the acting account from this header;
    Hyperliquid ignores it and uses the signature. *)
val headers : Config.t -> (string * string) list

val post
  :  Config.t
  -> ?sign:sign
  -> ?vault_address:Address.t
  -> Exchange_action.t
  -> Exchange_response.t Or_error.t Deferred.t

(** [build_request] for an action given as JSON, e.g. an {!Order_action} with a builder.
    [sign] sees the same unsigned body layout as for {!build_request}. *)
val build_envelope
  :  ?sign:sign
  -> ?vault_address:Address.t
  -> nonce:int
  -> Yojson.Safe.t
  -> Exchange_envelope.t

val post_envelope
  :  Config.t
  -> Exchange_envelope.t
  -> Exchange_response.t Or_error.t Deferred.t

(** {2 User-signed approvals}

    Signed with the account's own key (never an agent key) as EIP-712 typed data; see
    {!Signing.sign_user_action}. The signature is recovered and compared with the key's
    address before anything is sent. [X-Address] is the key's address, so the simulator
    credits the approval to the signer. Nonces come from {!next_nonce} unless given. *)

val post_user_signed
  :  Config.t
  -> key:Signing.Private_key.t
  -> User_signed_action.t
  -> Exchange_response.t Or_error.t Deferred.t

(** The signed body {!post_user_signed} sends. *)
val build_user_signed
  :  key:Signing.Private_key.t
  -> User_signed_action.t
  -> User_signed_request.t Or_error.t

(** The [agentName] for a named agent, with [" valid_until <ms>"] appended when
    [valid_until] is given. Per the exchange docs the expiry is at most 180 days ahead;
    later times, times not after [now] and empty names are rejected. *)
val agent_name : ?now:Time_ns.t -> ?valid_until:Time_ns.t -> string -> string Or_error.t

(** Approves [agent_address] as a named API wallet ("agent") of the key's account. Use a
    fresh agent address each time; never reuse one. *)
val approve_agent
  :  Config.t
  -> key:Signing.Private_key.t
  -> hyperliquid_chain:User_signed_action.Hyperliquid_chain.t
  -> ?nonce:int
  -> ?valid_until:Time_ns.t
  -> agent_address:Address.t
  -> agent_name:string
  -> unit
  -> Exchange_response.t Or_error.t Deferred.t

(** Approves [builder] to charge up to [max_fee_tenths_bp] (tenths of a basis point; perps
    cap it at 100, i.e. 0.1%) on orders that carry it. *)
val approve_builder_fee
  :  Config.t
  -> key:Signing.Private_key.t
  -> hyperliquid_chain:User_signed_action.Hyperliquid_chain.t
  -> ?nonce:int
  -> builder:Address.t
  -> max_fee_tenths_bp:int
  -> unit
  -> Exchange_response.t Or_error.t Deferred.t

(** {2 Helpers} *)

(** With [builder], the action is an {!Order_action} carrying [{"b", "f"}] (sent and
    signed through {!post_envelope}); the user must have approved at least that fee with
    {!approve_builder_fee}. Without it the request is byte-identical to {!post}'s. *)
val place_orders
  :  Config.t
  -> ?sign:sign
  -> ?vault_address:Address.t
  -> ?grouping:Grouping.t (** default [Na] *)
  -> ?builder:Builder_fee.t
  -> Order_request.t list
  -> Exchange_response.t Or_error.t Deferred.t

val cancel
  :  Config.t
  -> ?sign:sign
  -> ?vault_address:Address.t
  -> Cancel_request.t list
  -> Exchange_response.t Or_error.t Deferred.t

val cancel_by_cloid
  :  Config.t
  -> ?sign:sign
  -> ?vault_address:Address.t
  -> Cancel_by_cloid.t list
  -> Exchange_response.t Or_error.t Deferred.t

val modify
  :  Config.t
  -> ?sign:sign
  -> ?vault_address:Address.t
  -> Modify_request.t
  -> Exchange_response.t Or_error.t Deferred.t
