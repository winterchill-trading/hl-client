(** A typed handle over {!Config}: a [read_only t] can only reach [/info], a [trading t]
    can also POST to [/exchange].

    {[
      let feed = Client.read_only (Config.mainnet ()) in
      let%bind mids = Client.all_mids feed in
      (* Client.cancel feed [...] does not compile *)
      let%bind trader = Client.trading_of_env (Config.testnet ()) |> Deferred.return in
      ...
    ]}

    The untyped {!Info} and {!Exchange} functions over a bare [Config.t] stay available;
    this module is the checked entry point. *)

open! Core
open Async
open Hl_protocol

type read_only
type trading
type 'access t

(** Shows the config, whether a key is set (never the key) and the signing network. *)
val sexp_of_t : _ t -> Sexp.t

val read_only : Config.t -> read_only t

(** [trading ?key config] signs every exchange request with [key] when given (required by
    the real exchange; the simulator takes the account from [X-Address]). When [key] is
    given and the config has no address, the key's address becomes the default account.
    [is_mainnet] picks the signing domain and defaults to whether [base_url] is
    [api.hyperliquid.xyz]. *)
val trading : ?key:Signing.Private_key.t -> ?is_mainnet:bool -> Config.t -> trading t

(** A handle that trades for [account_address] with an approved API wallet ("agent").

    Per the Hyperliquid docs, an agent signs L1 actions (orders, cancels, modifies) for
    the account that approved it, and the exchange maps the signer to that account; no
    [vaultAddress] is set ([vaultAddress] is for vaults and subaccounts). The account
    address becomes the default [user] of info requests and the [X-Address] header for the
    simulator. Approvals cannot be signed with this handle. *)
val trading_with_agent
  :  ?is_mainnet:bool
  -> agent_key:Signing.Private_key.t
  -> account_address:Address.t
  -> Config.t
  -> trading t

(** [trading] with the key read from [HL_PRIVATE_KEY] (or [var]). *)
val trading_of_env : ?var:string -> ?is_mainnet:bool -> Config.t -> trading t Or_error.t

val config : _ t -> Config.t

(** The account requests act for ([Config.address]). *)
val account : _ t -> Address.t option

(** Whether the key is an agent's ({!trading_with_agent}). *)
val is_agent : _ t -> bool

(** ["Mainnet"] or ["Testnet"], from [is_mainnet]. *)
val hyperliquid_chain : _ t -> User_signed_action.Hyperliquid_chain.t

(** Whether requests are signed for mainnet (source ["a"]) rather than testnet. *)
val is_mainnet : _ t -> bool

val to_read_only : _ t -> read_only t

(** The [?sign] hook used for exchange requests, if the handle has a key. *)
val sign : trading t -> Exchange.sign option

(** {2 [/info], any handle} *)

val l2_book
  :  ?n_sig_figs:int
  -> ?mantissa:int
  -> _ t
  -> Coin.t
  -> L2_book.t Or_error.t Deferred.t

val all_mids : _ t -> All_mids.t Or_error.t Deferred.t
val open_orders : ?user:Address.t -> _ t -> Open_order.t list Or_error.t Deferred.t
val user_fills : ?user:Address.t -> _ t -> User_fill.t list Or_error.t Deferred.t

(** See {!Info.user_fills_by_time}. *)
val user_fills_by_time
  :  ?user:Address.t
  -> ?end_time:int
  -> ?aggregate_by_time:bool
  -> _ t
  -> start_time:int
  -> User_fill.t list Or_error.t Deferred.t

val clearinghouse_state
  :  ?user:Address.t
  -> _ t
  -> Clearinghouse_state.t Or_error.t Deferred.t

val meta : _ t -> Meta.t Or_error.t Deferred.t

(** {2 [/exchange], trading handles only} *)

(** The request [post] would send for [action] with [nonce], signature included. *)
val build_request
  :  ?vault_address:Address.t
  -> trading t
  -> nonce:int
  -> Exchange_action.t
  -> Exchange_request.t

val post
  :  ?vault_address:Address.t
  -> trading t
  -> Exchange_action.t
  -> Exchange_response.t Or_error.t Deferred.t

(** [builder]: see {!Exchange.place_orders}. *)
val place_orders
  :  ?vault_address:Address.t
  -> ?grouping:Grouping.t
  -> ?builder:Builder_fee.t
  -> trading t
  -> Order_request.t list
  -> Exchange_response.t Or_error.t Deferred.t

val cancel
  :  ?vault_address:Address.t
  -> trading t
  -> Cancel_request.t list
  -> Exchange_response.t Or_error.t Deferred.t

val cancel_by_cloid
  :  ?vault_address:Address.t
  -> trading t
  -> Cancel_by_cloid.t list
  -> Exchange_response.t Or_error.t Deferred.t

val modify
  :  ?vault_address:Address.t
  -> trading t
  -> Modify_request.t
  -> Exchange_response.t Or_error.t Deferred.t

(** {2 Approvals, owner handles only}

    Fail without sending when the handle has no key or holds an agent key. The network
    ([hyperliquidChain]) follows {!is_mainnet}. *)

(** See {!Exchange.approve_agent}. *)
val approve_agent
  :  ?nonce:int
  -> ?valid_until:Time_ns.t
  -> trading t
  -> agent_address:Address.t
  -> agent_name:string
  -> Exchange_response.t Or_error.t Deferred.t

(** See {!Exchange.approve_builder_fee}. *)
val approve_builder_fee
  :  ?nonce:int
  -> trading t
  -> builder:Address.t
  -> max_fee_tenths_bp:int
  -> Exchange_response.t Or_error.t Deferred.t
