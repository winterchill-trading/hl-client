# AGENTS.md: hl-client/lib

The `hl-client` library (module `Hl_client`).

| Module | Holds |
| --- | --- |
| `Config` | `base_url`, `ws_url`, the default address; `local`, `mainnet` and `testnet` presets |
| `Info` | `POST /info` calls: order book, mids, open orders, fills (with pagers), clearinghouse state, metadata |
| `Exchange` | `POST /exchange` calls: place, cancel, cancel by client id, modify |
| `Signing` | EIP-712 and action signing, checked against the SDK vectors |
| `Client` | Typed handles over the calls |
| `Ws` | Websocket subscriptions and message encoding |
| `Live_feed` | Normalized market data with reconnects, `stats`, `feed_with_gaps` |
| `Normalizer`, `Raw_normalizer` | Frame to event conversion, the latter as a `Market_data.Normalizer.S` |
| `Frame_scan` | Hand-written zero-allocation JSON scanners for the hot frames, with the Yojson path as fallback |
| `Keccak`, `Msgpack` | Hashing and MessagePack encoding used by signing |
| `Http`, `User_feed`, `Leaderboard` | Transport helpers and the other-account and leaderboard readers |

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @hl-client/all`.

Rules:
- A scanner change keeps a test that both the scanner and the Yojson path produce the same
  values on the same frames.
- Signing code does not log its inputs or outputs.
- Keep `Live_feed` free of order logic; it only reads.
