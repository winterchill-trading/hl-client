# AGENTS.md: hl-client/bin

The `hl-cli` command (`hl_cli.ml`): `l2book`, `mids`, `watch`, `feed`, `address`, `cancel`.
The flags are in `../README.md`.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @hl-client/all`.

Rules:
- `cancel` refuses mainnet without `-allow-mainnet`. Keep that check first in the command.
- Commands that only read must not need a key.
