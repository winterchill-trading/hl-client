(** The [POST /exchange] endpoint.

    Signing is out of scope: requests carry an optional opaque [signature] produced by the
    [?sign] hook. The simulator accepts unsigned requests; the real Hyperliquid API
    rejects them. *)

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

(** {2 Helpers} *)

val place_orders
  :  Config.t
  -> ?sign:sign
  -> ?vault_address:Address.t
  -> ?grouping:Grouping.t (** default [Na] *)
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
