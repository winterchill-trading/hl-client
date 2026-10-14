# AGENTS.md: hl-client/test

Expect tests run with no network: request encodings, response decoding, signing vectors,
typed handles, websocket encoding, normalization of captured frames, the frame scanners
against the Yojson path, words per frame, `rebuild-norm` over a recorded minute, and loopback
websocket tests on `127.0.0.1`. `test_reconnect_fake_clock.ml` steps backoff on a synthetic
`Time_source`.

- `blast/` is a load driver (`blast_server.ml`, `feed_cost.ml`); it is not a unit test.
- `fixtures/` holds a recorded mainnet minute in the raw layout, one file per stream, and
  documentation captures for user fills.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @hl-client/runtest`.

Rules:
- Promote only after reading the diff and after a successful `@runtest` build.
- A fixture is a recorded frame. Do not edit it by hand; record a new one.
- Copy fixtures with `cp -RL`; dune's sandbox holds them as symlinks.
- A pinned word count carries a comment with the platform that measured it.
