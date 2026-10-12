# hl-client

Async client for the Hyperliquid HTTP and websocket API

Built with [OxCaml](https://oxcaml.org) and the Jane Street library stack (Core, Async),
on top of the wire types in [hl-protocol](https://github.com/winterchill-trading/hl-protocol)
and the venue-independent transport in
[venue-net](https://github.com/winterchill-trading/venue-net).
It targets the local simulator ([hl-sim](https://github.com/winterchill-trading/hl-sim))
and the real Hyperliquid API. Request signing is out of scope: the read-only parts
(`Info`, `Ws`) work against Hyperliquid as is, while `Exchange` sends unsigned requests
unless you pass a `~sign` hook.

## Build

```sh
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
opam pin add -y -n fixed-decimal https://github.com/winterchill-trading/fixed-decimal.git
opam pin add -y -n exchange-core https://github.com/winterchill-trading/exchange-core.git
opam pin add -y -n market-data https://github.com/winterchill-trading/market-data.git
opam pin add -y -n hl-protocol https://github.com/winterchill-trading/hl-protocol.git
opam pin add -y -n venue-net https://github.com/winterchill-trading/venue-net.git
opam install . --deps-only --with-test
dune build && dune test
```

`https://` and `wss://` need `async_ssl` (OpenSSL 3). On macOS with Homebrew's
`openssl@4` linked into `/opt/homebrew/lib`, build `async_ssl` against `openssl@3` and
make sure it links by absolute path (see "macOS and OpenSSL" below).

## Quick start

```ocaml
open! Core
open Async
open Hl_protocol

let main () =
  let config = Hl_client.Config.local () in            (* http://localhost:3000 *)
  let%bind book = Hl_client.Info.l2_book config (Coin.of_string "BTC") in
  print_s [%sexp (Or_error.ok_exn book : L2_book.t)];
  let%bind ws = Hl_client.Ws.connect config >>| ok_exn in
  let%bind () = Hl_client.Ws.subscribe ws (L2_book { coin = Coin.of_string "BTC" }) in
  Pipe.iter_without_pushback (Hl_client.Ws.messages ws) ~f:(fun message ->
    print_s [%sexp (message : Ws_message.t)])
```

## API

All modules live under `Hl_client`. Every network call returns `_ Or_error.t
Deferred.t`; transport failures, non-2xx statuses and decoding failures become `Error`
(carrying the HTTP status and body where there is one). Note that `open Hl_client`
shadows `Core.Info` with `Hl_client.Info`.

### `Config`

```ocaml
type t = { base_url : Uri.t; ws_url : Uri.t; address : Address.t option }

val create   : ?ws_url:Uri.t -> ?address:Address.t -> base_url:Uri.t -> unit -> t
val local    : ?address:Address.t -> unit -> t   (* http://localhost:3000 *)
val mainnet  : ?address:Address.t -> unit -> t   (* https://api.hyperliquid.xyz *)
val testnet  : ?address:Address.t -> unit -> t   (* https://api.hyperliquid-testnet.xyz *)
val info_uri : t -> Uri.t
val exchange_uri : t -> Uri.t
```

`ws_url` defaults to `base_url` with scheme `ws`/`wss` and path `/ws`. `address` is the
default `user` for account-scoped info requests.

### `Info` (`POST /info`)

```ocaml
val post : Config.t -> Info_request.t -> Info_response.t Or_error.t Deferred.t

val l2_book : ?n_sig_figs:int -> ?mantissa:int -> Config.t -> Coin.t -> L2_book.t Or_error.t Deferred.t
val all_mids : Config.t -> All_mids.t Or_error.t Deferred.t
val open_orders : ?user:Address.t -> Config.t -> Open_order.t list Or_error.t Deferred.t
val user_fills : ?user:Address.t -> Config.t -> User_fill.t list Or_error.t Deferred.t
val clearinghouse_state : ?user:Address.t -> Config.t -> Clearinghouse_state.t Or_error.t Deferred.t
val meta : Config.t -> Meta.t Or_error.t Deferred.t
```

`?user` defaults to `Config.address`; the call fails if neither is set. `request_body`
and `decode` expose the pure encoding and decoding steps.

### `Exchange` (`POST /exchange`)

```ocaml
type sign = Yojson.Safe.t -> Yojson.Safe.t option

val post : Config.t -> ?sign:sign -> ?vault_address:Address.t -> Exchange_action.t
  -> Exchange_response.t Or_error.t Deferred.t

val place_orders : Config.t -> ?sign:sign -> ?vault_address:Address.t
  -> ?grouping:Grouping.t -> Order_request.t list -> Exchange_response.t Or_error.t Deferred.t
val cancel : ... -> Cancel_request.t list -> ...
val cancel_by_cloid : ... -> Cancel_by_cloid.t list -> ...
val modify : ... -> Modify_request.t -> ...
```

- When `Config.address` is set, every request carries an `X-Address` header with it.
  The simulator takes the acting account from that header; Hyperliquid ignores it.
- The nonce is the current time in milliseconds, forced strictly increasing within the
  process (`next_nonce`), so two requests in the same millisecond do not collide.
- `sign` receives the unsigned request body (`{"action", "nonce", "vaultAddress"?}`)
  and returns the `signature` JSON, or `None` to send unsigned. Without `sign` the
  `signature` field is omitted; the simulator accepts that, Hyperliquid does not.
- A response that decodes as `{"status": ...}` is returned as `Ok` whatever the HTTP
  status; `Exchange_response.Err msg` is a successful call with a rejected action.
- Per-order outcomes are in `Exchange_response.Ok (Order statuses)`.
- `build_request` and `decode` expose the pure steps.

### `Ws` (websocket)

```ocaml
val connect
  :  ?ping_interval:Time_float.Span.t     (* default 50s *)
  -> ?initial_backoff:Time_float.Span.t   (* default 0.5s *)
  -> ?max_backoff:Time_float.Span.t       (* default 30s *)
  -> ?on_reconnect:(unit -> unit)
  -> Config.t
  -> t Or_error.t Deferred.t

val messages : t -> Ws_message.t Pipe.Reader.t
val subscribe : t -> Subscription.t -> unit Deferred.t
val unsubscribe : t -> Subscription.t -> unit Deferred.t
val subscriptions : t -> Subscription.t list
val send : t -> Ws_request.t -> unit Deferred.t
val is_connected : t -> bool
val close : t -> unit Deferred.t
```

- `connect` returns once the first handshake succeeds (or fails). After that the
  connection is maintained in the background.
- `{"method": "ping"}` is sent every `ping_interval` (Hyperliquid closes connections
  that are silent for 60s). If nothing at all arrives during two consecutive intervals,
  the socket is closed and replaced.
- On close, the client reconnects with exponential backoff (doubling from
  `initial_backoff` up to `max_backoff`, reset after a successful handshake), resends
  every active subscription, then calls `on_reconnect`. The server sends fresh
  snapshots after resubscription; use `on_reconnect` to resync anything else (e.g.
  open orders via `Info.open_orders`), since updates during the gap are lost.
- `messages` carries every decoded message from all connections, including
  `Subscription_response`, `Pong`, `Error` and `Unknown` channels (logged at debug).
  Frames that fail to decode are logged at debug level and dropped. Messages are
  buffered without pushback, so keep reading.
- `subscribe` is idempotent and works while disconnected (the subscription is sent on
  the next connection).
- `ws://` uses plain TCP; `wss://` uses OpenSSL with SNI set to the URL's host.
- The transport (handshake, keepalive, watchdog, backoff) is `Venue_net.Ws_client`,
  configured with a `{"method":"ping"}` text keepalive and a one-interval silence limit;
  this module adds the typed messages and the subscription set. `Http` likewise wraps
  `Venue_net.Http`.

## CLI

`hl-cli` (`bin/hl_cli.ml`) is a smoke-testing tool:

```sh
hl-cli l2book BTC                                   # simulator on localhost:3000
hl-cli mids -base-url https://api.hyperliquid.xyz
hl-cli watch -l2book BTC -trades BTC -all-mids      # prints every websocket message
hl-cli watch -order-updates 0x... -user-fills 0x... -ws-url ws://localhost:3000/ws -debug
```

## Tests

`dune test` runs ppx_expect tests with no network access: request encodings, response
decoding (including error cases), nonce ordering, websocket message encoding and
decoding, and a loopback test that runs a websocket
server on `127.0.0.1` to check subscribe, server-initiated close, reconnect,
resubscription, ping/pong, silent-connection detection and shutdown. The handshake
(RFC 6455 vectors), backoff and keepalive variants are tested in venue-net.

## macOS and OpenSSL

`async_ssl` does not compile against OpenSSL 4 (`ASN1_STRING_data` was removed). If
Homebrew's `openssl@4` is linked, a plain `-lssl` can also resolve to it at link time,
because other C libraries (e.g. zarith, pulled in by `async_websocket` via
`cryptokit`) put `-L/opt/homebrew/lib` first. A pkg-config file that names the
`openssl@3` libraries by absolute path avoids both problems:

```sh
D=$(opam var prefix)/share/openssl3-pkgconfig; O=/opt/homebrew/opt/openssl@3
mkdir -p $D && cat > $D/openssl.pc <<PC
Name: OpenSSL
Description: openssl@3
Version: 3
Libs: -L$O/lib $O/lib/libssl.dylib $O/lib/libcrypto.dylib
Cflags: -I$O/include
PC
PKG_CONFIG_PATH=$D opam reinstall async_ssl
```

The first `Libs` entry must start with `-`, since dune reads it as an ordered set.
