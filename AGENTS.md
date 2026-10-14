# AGENTS.md: hl-client

## Purpose

Async client for the Hyperliquid HTTP and websocket API, usable against `hl-sim` (local) and
the real API (testnet or mainnet), plus a normalized `Live_feed` for the data platform.
Signing follows the SDK test vectors. Order placement on mainnet is guarded.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The library `hl-client` (module `Hl_client`): config, info and exchange calls, signing, websocket, live feed, normalizer, frame scanner (see `lib/AGENTS.md`) |
| `bin/` | The `hl-cli` command: `l2book`, `mids`, `watch`, `feed`, `address`, `cancel` (see `bin/AGENTS.md`) |
| `bench/` | `feed/`, the feed decode benchmark (see `bench/AGENTS.md`) |
| `test/` | Expect and property tests; `test/blast/` is a load driver; `test/fixtures/` holds recorded raw minutes and docs captures (see `test/AGENTS.md`) |
| `.github/` | CI workflow and the shared obj-magic check (see `.github/AGENTS.md`) |
| `README.md` | API by module, CLI, tests, macOS and OpenSSL notes |
| `dune-project` | Package metadata; edit this, not the generated `hl-client.opam` |

## Build, test

Run from this repository. The workspace root is one level up, so every command takes
`--root ..`.

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @hl-client/all
opam exec --switch 5.2.0+ox -- dune build --root .. @hl-client/runtest
```

Parallel work with another agent: prefix each with `DUNE_BUILD_DIR=_build-hl-client`.

Tests run without network access. Loopback tests bind `127.0.0.1`.

Standalone (outside the workspace): the org opam repository is the target (see
`../specs/repos/opam-repository.md`). Until then pin `fixed-decimal`, `exchange-core`,
`parquet`, `market-data`, `hl-protocol` and `venue-net` from GitHub in the CI order, then
`opam install . --deps-only --with-test` and `dune build`. On macOS with Homebrew OpenSSL
read the README section first.

## Conventions

- Format only this repository: `dune build --root .. @hl-client/fmt --auto-promote`. Never
  run `dune fmt` at the workspace root.
- Every module has an `.mli`. Consume siblings by public opam name.
- Signing is tested against the reference SDK vectors and the testnet. A change to signing
  adds or updates a vector in the same commit.
- Mainnet is guarded: `hl-cli cancel` refuses mainnet without `-allow-mainnet`. Do not add
  a path that places or cancels on mainnet without that flag.
- Allocation pins (`test/`): word counts differ between macOS arm64 and Linux amd64. Each
  pin carries a comment saying which platform measured it.
- Comments for humans at non-obvious choices (the Hyperliquid wire quirks, reconnect gaps).

## Dependencies and dependents

Sibling checkouts in CI: `fixed-decimal`, `exchange-core`, `parquet`, `market-data`,
`hl-protocol`, `venue-net`, in that order (`.github/workflows/ci.yml`). `../hl-sim` is the local server it is tested against.

## CI

`.github/workflows/ci.yml`: checkout, the shared `ox-setup` action (its cache is keyed on
the pins; a partial hit reinstalls the system packages), `dune build`, `dune test`, then the
reusable obj-magic workflow from `../infra`. Pushes cancel the running job.

To check a run: one `gh run list --limit 1`, then at most one `gh run watch <id>
--exit-status`. Never poll in a loop.

## Pitfalls

- The switch needs the OxCaml preview of `async_websocket`. A plain `opam install` can pick
  the empty 0.13 stub, which has no modules.
- `async_ssl` does not build against OpenSSL 4 and can link the wrong OpenSSL; the README's
  macOS section has the workaround. Without `async_ssl`, the conduit layer has no TLS and
  fails silently.
- `Conduit_async` builds a `wss` configuration without the SNI host name. Rebuild it with the
  host name set.
- A library module called `Info` shadows `Core.Info` for siblings and for `open Hl_client`.
  Write `Core.Info` explicitly.
- Under OxCaml `Base`, `Random.State.make_self_init` raises in inline tests. Seed from
  `Time_ns.now`.
- Base's `List.init` calls the function from the last index down; do not depend on order.
- ppx_yojson error text embeds the source path, which differs between workspace and opam
  builds. Keep it out of expect output.
- `dune build` can hold `_build/.lock` for a long time with no child process; wrap agent
  builds in `timeout`.

## Where to read more

- `README.md`: the API by module, the CLI, tests, the macOS and OpenSSL notes, and the
  reconnect frame-loss section.
- `../specs/repos/hl-client.md`: status and consumers.
- `../hl-protocol/`: the wire types this client decodes.
- `../venue-net/`: the websocket and HTTP transport underneath.
- `../oxcaml-claude-skills/skills/oxcaml-performance/`: the frame scanner's method.
- `../oxcaml-claude-skills/AGENTS.md`: workspace-wide guide.
- `../oxcaml-claude-skills/GOTCHAS.md`: the hl-client and switch sections.

## Never do

- Place, sign or cancel on mainnet from a test, a script or a default command.
- Commit a private key or a signing secret. Read keys from the environment.
- Install opam packages that change the compiler or `core`, `base`, `async`, `ppx_jane`
  on the shared `5.2.0+ox` switch. Dry-run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`.
- Publish, tag or release.
- Poll CI in loops.
