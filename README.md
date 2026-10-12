# hl-client

Async client for the Hyperliquid HTTP and websocket API

Built with [OxCaml](https://oxcaml.org) and the Jane Street library stack (Core, Async),
on top of the wire types in [hl-protocol](https://github.com/winterchill-trading/hl-protocol)
and the venue-independent transport in
[venue-net](https://github.com/winterchill-trading/venue-net).
It targets the local simulator ([hl-sim](https://github.com/winterchill-trading/hl-sim))
and the real Hyperliquid API. Exchange requests are signed like the official Python SDK
does it (EIP-712 over a MessagePack action hash, secp256k1); `Client` handles are typed
so that a read-only handle cannot reach `/exchange`.

## Build

```sh
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
opam pin add -y -n fixed-decimal https://github.com/winterchill-trading/fixed-decimal.git
opam pin add -y -n exchange-core https://github.com/winterchill-trading/exchange-core.git
opam pin add -y -n parquet https://github.com/winterchill-trading/parquet.git
opam pin add -y -n market-data https://github.com/winterchill-trading/market-data.git
opam pin add -y -n hl-protocol https://github.com/winterchill-trading/hl-protocol.git
opam pin add -y -n venue-net https://github.com/winterchill-trading/venue-net.git
opam install . --deps-only --with-test
dune build && dune test
```

Signing uses the `secp256k1` opam binding, which needs the C library
(`brew install secp256k1`, or `libsecp256k1-dev` on Debian and Ubuntu; opam installs it
as a depext). Keccak-256 and the MessagePack encoder are pure OCaml in this repo, since
`digestif` 1.3 does not build on OxCaml.

`https://` and `wss://` need `async_ssl` (OpenSSL 3). On macOS with Homebrew's
`openssl@4` linked into `/opt/homebrew/lib`, build `async_ssl` against `openssl@3` and
make sure it links by absolute path (see "macOS and OpenSSL" below).

CI installs the pinned siblings and the opam dependencies through the shared
[ox-setup](https://github.com/winterchill-trading/infra/blob/main/docs/ci.md) action, which
caches the built opam switch between runs.

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

### `Signing`

```ocaml
module Private_key : sig
  type t                                         (* sexp shows "<private key>" *)
  val of_hex : string -> t Or_error.t            (* 64 hex digits, 0x optional *)
  val of_env : ?var:string -> unit -> t Or_error.t   (* default HL_PRIVATE_KEY *)
  val address : t -> Address.t
end

module Signature : sig type t = { r : string; s : string; v : int } end

val action_hash : ?expires_after:int -> vault_address:Address.t option -> nonce:int
  -> Yojson.Safe.t -> string
val l1_digest : is_mainnet:bool -> connection_id:string -> string
val sign_l1_action : ?expires_after:int -> Private_key.t -> is_mainnet:bool
  -> vault_address:Address.t option -> nonce:int -> Yojson.Safe.t -> Signature.t
val recover : digest:string -> Signature.t -> Address.t Or_error.t
val sign_request : Private_key.t -> is_mainnet:bool -> Exchange.sign
```

L1 actions (order, cancel, cancelByCloid, modify, and any other action JSON) are signed
as in `hyperliquid/utils/signing.py`:

1. `action_hash` = Keccak-256 of `msgpack(action) ++ nonce (8 bytes, big endian) ++
   (0x00 | 0x01 ++ vault address) [++ 0x00 ++ expiresAfter (8 bytes)]`. Key order in the
   action JSON is hashed as given; the `Hl_protocol` encoders emit the exchange's order
   (`type` first, orders as `a b p s r t c`).
2. The phantom agent `{source: "a" | "b", connectionId: action_hash}` (mainnet | testnet)
   is hashed as EIP-712 typed data in the domain `Exchange`, version `1`, chain id 1337,
   verifying contract `0x0`.
3. The digest is signed with secp256k1 (RFC 6979, low s); `v` is `27 + recovery id`, and
   `r`, `s` are hex without leading zeros, as the SDK sends them.

The tests reproduce every L1 vector of the SDK's `tests/signing_test.py` (orders with and
without cloid, trigger orders, vault, `createSubAccount`, `scheduleCancel`, and the
production phantom-agent hash). A live check against testnet with a freshly generated
key gave `User or API Wallet 0x04c6...dab5 does not exist.`, i.e. the exchange recovered
the address the key derives, so the signature is accepted as that account's. User-signed
actions (transfers, withdrawals) are not implemented.

### `Client` (typed handles)

```ocaml
type read_only
type trading
type 'access t

val read_only : Config.t -> read_only t
val trading : ?key:Signing.Private_key.t -> ?is_mainnet:bool -> Config.t -> trading t
val trading_of_env : ?var:string -> ?is_mainnet:bool -> Config.t -> trading t Or_error.t
val to_read_only : _ t -> read_only t

val l2_book : ... -> _ t -> ...          (* every Info helper, any handle *)
val place_orders : ... -> trading t -> ...   (* post, cancel, cancel_by_cloid, modify *)
```

`Client.cancel (Client.read_only config) [...]` is a type error. A trading handle with a
key signs every request (the key's address becomes the default `user` when the config has
none); without a key it sends unsigned requests, which is what the simulator wants.
`is_mainnet` defaults to whether the host is `api.hyperliquid.xyz`. The untyped `Info`
and `Exchange` functions over a bare `Config.t` stay as they were.

### `Ws` (websocket)

```ocaml
val connect
  :  ?ping_interval:Time_float.Span.t     (* default 50s *)
  -> ?initial_backoff:Time_float.Span.t   (* default 0.5s *)
  -> ?max_backoff:Time_float.Span.t       (* default 30s *)
  -> ?backoff_jitter:float                (* default 0.2 *)
  -> ?time_source:Time_source.t           (* default the wall clock *)
  -> ?on_reconnect:(unit -> unit)
  -> Config.t
  -> t Or_error.t Deferred.t

val messages : t -> Ws_message.t Pipe.Reader.t
val subscribe : t -> Subscription.t -> unit Deferred.t
val unsubscribe : t -> Subscription.t -> unit Deferred.t
val subscriptions : t -> Subscription.t list
val send : t -> Ws_request.t -> unit Deferred.t
val is_connected : t -> bool
val reconnects : t -> int
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

### `Live_feed` (normalized market data)

```ocaml
val create
  :  ?on_raw:(stream:Market_data.Stream.t -> recv_ts:Time_ns.t -> string -> unit)
  -> ?time_source:Time_source.t
  -> Config.t -> symbols:string list -> streams:Market_data.Stream.t list
  -> Market_data.Feed.t Deferred.t

module Stats : sig
  type t = { frames : int; parse_errors : int; error_frames : int; reconnects : int }
end

val create_with_stats : (* same arguments *) -> (Market_data.Feed.t * (unit -> Stats.t)) Deferred.t

type t
val start          : (* same arguments *) -> t Deferred.t
val feed           : t -> Market_data.Feed.t
val feed_with_gaps : t -> Market_data.Feed.With_gaps.t
val stats          : t -> Stats.t
```

A read-only [market-data](https://github.com/winterchill-trading/market-data) feed over
the public websocket, for the simulator and the real Hyperliquid API. Symbols are coins
(`BTC`); instruments are `hyperliquid:<coin>`.

| Stream    | Channel          | Notes                                                        |
|-----------|------------------|--------------------------------------------------------------|
| `Trades`  | `trades`         | `exch_ts` = `time`, `trade_id` = `tid`, side = aggressor     |
| `L2`      | `l2Book`         | every message is a full book, so always a `Snapshot`         |
| `Bbo`     | `l2Book`         | top of book, emitted when it changes                         |
| `Funding` | `activeAssetCtx` | `funding`, `markPx`, `oraclePx` (index); no venue timestamp, so `exch_ts` = `recv_ts`; next funding on the hour. Not served by the simulator |

`recv_ts` is taken from `time_source` (default the wall clock) when the frame is read,
and `on_raw` sees each data frame exactly once, before it is parsed. An `l2Book` frame
feeds both `L2` and `Bbo` but is passed once, under `L2` when `L2` is requested and under
`Bbo` otherwise, so a recorder stores it once. Reconnects use `Venue_net.Ws_client`
backoff and resubscribe, which brings a fresh book snapshot. Unparsable frames are logged
and dropped; `create_with_stats` counts them, along with server error frames and
reconnects. Pipes of streams not requested are
closed; the others are unbounded, so read them all.

Every reconnect is reported on the `gaps` pipe of `feed_with_gaps` as one
`Market_data.Gap.t` with reason `Resync`, venue `Hyperliquid` and no instrument or stream
(the break covers the whole connection). Its `recv_ts` is when the new connection came up,
and it is written before the resubscriptions go out, so it precedes the fresh snapshot
and bbo. It is also counted in `Stats.reconnects`. The `gaps` pipe is written without
pushback and closes with the feed, so it can be left unread.

#### Reconnect frame loss

Nothing the client has read is dropped: frames of the old connection already in the
`Venue_net.Ws_client` pipe are still normalized and delivered. What is lost is every frame
the venue publishes after the last one the old connection delivered and before the
resubscription on the new connection is acknowledged. That window is

1. detection: immediate when the server closes the socket; when the connection stalls
   silently, up to one keepalive interval (50 s, `max_silent_intervals` 1), since the
   watchdog closes the socket after an interval with no incoming frame or pong;
2. the backoff sleep: 0.5 s with 20% jitter (400 to 600 ms) for the first attempt,
   doubling per failed attempt up to 30 s;
3. the TCP and TLS handshake, the websocket upgrade and the subscribe round trip.

Per stream, inside the window:

| Stream    | Lost                                   | Recovered by                                   |
|-----------|----------------------------------------|------------------------------------------------|
| `L2`      | intermediate books                     | the first `l2Book` after resubscribe (a full snapshot) |
| `Bbo`     | top-of-book changes                    | the same snapshot; the dedup state is reset, so the first bbo is always emitted |
| `Trades`  | every trade printed in the window      | nothing in this client (no backfill)           |
| `Funding` | intermediate `activeAssetCtx` updates  | the next update                                |

Measured on the synthetic clock in `test_reconnect_fake_clock.ml` (loopback server, socket
I/O does not advance the clock): the last frame of the first connection is stamped 0 ms,
the server drops the socket, and the reconnect sleep is 400 to 600 ms (412 ms in one
run, 414 ms for the following reconnect). Stepping the clock exactly to the end of the
sleep stamps the `Resync` gap and the fresh snapshot with the same `recv_ts`, so the
window is the backoff alone. In the stats test, which advances the clock by a whole
second, the raw frames are stamped 0, 0 and 1000 ms: a 1 s hole between the last frame
of the first connection and the snapshot of the second. On a real network add the
handshake and subscribe round trip (item 3) to the backoff.

### `Normalizer` and `Raw_normalizer`

```ocaml
(* Normalizer *)
type t
val create : ?streams:Market_data.Stream.t list -> unit -> t   (* default all four *)
val frame : t -> recv_ts:Time_ns.t -> string -> Market_data.Event.t list Or_error.t
val reset : t -> unit

(* Raw_normalizer *)
include Market_data.Normalizer.S with type t = Normalizer.t
```

`Normalizer` is the pure frame-to-events code that `Live_feed` runs on every frame,
including the top-of-book de-duplication of `Bbo`; `Live_feed` resets it on every
(re)connection. `Raw_normalizer` adapts it to `Market_data.Normalizer.S` (all streams;
`disconnect` resets), which is what `Market_data.Rebuild_norm` needs to rebuild `norm/`
event logs from market-recorder raw minute files.

## CLI

`hl-cli` (`bin/hl_cli.ml`) is a smoke-testing tool:

```sh
hl-cli l2book BTC                                   # simulator on localhost:3000
hl-cli mids -base-url https://api.hyperliquid.xyz
hl-cli watch -l2book BTC -trades BTC -all-mids      # prints every websocket message
hl-cli watch -order-updates 0x... -user-fills 0x... -ws-url ws://localhost:3000/ws -debug
hl-cli feed -symbol BTC -streams trades,bbo         # normalized market-data events
hl-cli feed -base-url https://api.hyperliquid.xyz -symbol BTC -streams trades,bbo,l2,funding -raw
hl-cli rebuild-norm -data-dir /data -date 2026-10-12 -out /tmp/norm  # raw minutes -> event logs
HL_PRIVATE_KEY=0x... hl-cli address                 # account address of the key
HL_PRIVATE_KEY=0x... hl-cli cancel -base-url https://api.hyperliquid-testnet.xyz -asset 0 -oid 1
```

`cancel` signs with `HL_PRIVATE_KEY` and refuses `api.hyperliquid.xyz` unless given
`-allow-mainnet`.

`feed` prints `Market_data.Event.t` sexps on stdout (`-raw` also prints each frame on
stderr). Checked against the simulator with

```sh
(cd ../hl-sim && dune exec hl-sim -- -port 3000) &
dune exec hl-cli -- feed -symbol BTC -streams trades,bbo
```

```
(Bbo
 ((instrument ((venue Hyperliquid) (symbol BTC)))
  (exch_ts (2026-10-12 12:16:07.633000000+01:00))
  (recv_ts (2026-10-12 12:16:07.662333000+01:00)) (bid_price 100007)
  (bid_size 0.01341) (ask_price 100062) (ask_size 0.01492)))
(Trade
 ((instrument ((venue Hyperliquid) (symbol BTC)))
  (exch_ts (2026-10-12 12:16:07.844000000+01:00))
  (recv_ts (2026-10-12 12:16:07.844605000+01:00)) (price 100007)
  (size 0.01196) (side Sell) (trade_id 14)))
```

## Tests

`dune test` runs ppx_expect tests with no network access: request encodings, response
decoding (including error cases), nonce ordering, Keccak-256 and MessagePack vectors,
the SDK signing vectors, typed handles, websocket message encoding and decoding,
`Normalizer` and `Live_feed` normalization of captured frames, `rebuild-norm` over the
recorded minute, and loopback tests that run a websocket
server on `127.0.0.1` to check subscribe, server-initiated close, reconnect,
resubscription, ping/pong, silent-connection detection and shutdown. The reconnect tests
in `test_reconnect_fake_clock.ml` step the backoff on a synthetic `Time_source` (no
reconnect at 990 ms of a 1 s backoff, one at 1 s) and check the `Live_feed` stats.

`test/fixtures/raw/hyperliquid/<stream>/BTC/2026/10/12/13-35.ndjson` is a trimmed minute
recorded from mainnet by market-recorder, in its raw layout (`<recv_ts> <frame>` per
line, one file per stream: 25 trades
frames, 6 `l2Book`, 12 `activeAssetCtx`). Every frame decodes; normalized, they give 40
trades, 6 L2 snapshots, 6 bbo and 12 funding events, and `rebuild-norm` writes the same
64 events into four event logs. The handshake (RFC 6455 vectors),
backoff and keepalive variants are tested in venue-net.

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
