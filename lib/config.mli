(** Where to reach a Hyperliquid-compatible API, and which account to act for. *)

open! Core
open Hl_protocol

type t =
  { base_url : Uri.t (** HTTP root; [/info] and [/exchange] are resolved against it *)
  ; ws_url : Uri.t (** websocket endpoint, e.g. [ws://localhost:3000/ws] *)
  ; address : Address.t option (** default [user] for account-scoped info requests *)
  }
[@@deriving sexp_of]

(** [create ~base_url ()] derives [ws_url] from [base_url] unless given: the scheme
    [http]/[https] becomes [ws]/[wss] and the path becomes [/ws]. *)
val create : ?ws_url:Uri.t -> ?address:Address.t -> base_url:Uri.t -> unit -> t

(** The local simulator, [http://localhost:3000]. *)
val local : ?address:Address.t -> unit -> t

(** [https://api.hyperliquid.xyz]. *)
val mainnet : ?address:Address.t -> unit -> t

(** [https://api.hyperliquid-testnet.xyz]. *)
val testnet : ?address:Address.t -> unit -> t

val info_uri : t -> Uri.t
val exchange_uri : t -> Uri.t

(** [ws_url_of_base_url base_url] is the default websocket URL for [base_url]. *)
val ws_url_of_base_url : Uri.t -> Uri.t
