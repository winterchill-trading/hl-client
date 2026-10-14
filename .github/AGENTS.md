# AGENTS.md: hl-client/.github

- `workflows/ci.yml`: checkout, the shared `ox-setup` action (cache keyed on the pins list),
  `dune build`, `dune test`, then the reusable obj-magic workflow from
  `winterchill-trading/infra`.
- The pins list must match the sibling dependencies in `dune-project`, in dependency order.
  After a pin change the first run is a full cache miss; expect it to be slow.
- Concurrency is `cancel-in-progress: true`; push once per finished change.
- To check a run: one `gh run list --limit 1`, then at most one `gh run watch <id>
  --exit-status`. Never poll.
