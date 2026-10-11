(** The [POST /info] endpoint. Read-only; works against the simulator and the real
    Hyperliquid API.

    Note that this module shadows [Core.Info] after [open Hl_client]. *)

open! Core
open Async
open Hl_protocol

(** The JSON body sent for a request. *)
val request_body : Info_request.t -> Yojson.Safe.t

(** Decode a response body given the request that produced it. *)
val decode : Info_request.t -> Http.Response.t -> Info_response.t Or_error.t

val post : Config.t -> Info_request.t -> Info_response.t Or_error.t Deferred.t

(** {2 Typed helpers}

    Account-scoped requests take [?user], defaulting to [Config.address]; they fail if
    neither is set. *)

val l2_book
  :  ?n_sig_figs:int
  -> ?mantissa:int
  -> Config.t
  -> Coin.t
  -> L2_book.t Or_error.t Deferred.t

val all_mids : Config.t -> All_mids.t Or_error.t Deferred.t
val open_orders : ?user:Address.t -> Config.t -> Open_order.t list Or_error.t Deferred.t
val user_fills : ?user:Address.t -> Config.t -> User_fill.t list Or_error.t Deferred.t

val clearinghouse_state
  :  ?user:Address.t
  -> Config.t
  -> Clearinghouse_state.t Or_error.t Deferred.t

val meta : Config.t -> Meta.t Or_error.t Deferred.t
