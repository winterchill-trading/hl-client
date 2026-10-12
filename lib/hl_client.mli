(** Async client for the Hyperliquid HTTP and websocket API, built on the [Hl_protocol]
    wire types.

    {[
      let config = Hl_client.Config.local () in
      let%bind book = Hl_client.Info.l2_book config (Hl_protocol.Coin.of_string "BTC") in
      ...
    ]}

    [open Hl_client] shadows [Core.Info] with {!Info}; refer to [Core.Info] explicitly
    afterwards, or use qualified names. *)

(** Endpoints and default account. *)
module Config = Config

(** JSON-over-HTTP transport shared by [Info] and [Exchange]. *)
module Http = Http

(** [POST /info]: books, mids, open orders, fills, positions, metadata. *)
module Info = Info

(** [POST /exchange]: place, cancel and modify orders. Signing is delegated to a hook. *)
module Exchange = Exchange

(** Websocket subscriptions with ping, reconnect and resubscription. *)
module Ws = Ws

(** Normalized market data ({!Market_data.Feed.t}) from the public websocket. *)
module Live_feed = Live_feed

(** Keccak-256, for Ethereum-style request signing. *)
module Keccak = Keccak

(** MessagePack encoding of actions, hashed when signing. *)
module Msgpack = Msgpack

(** EIP-712 signing of exchange actions with a secp256k1 key. *)
module Signing = Signing
