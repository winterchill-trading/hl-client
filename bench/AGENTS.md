# AGENTS.md: hl-client/bench

`feed/` is the feed decode benchmark (`feed.ml`, executable `feed`, declared in `bench/dune`).
It reports the cost of the frame path in ns and words per frame.

Rules:
- Report words per frame and name the platform when a number goes into the README.
- The benchmark is not a test; do not use its numbers as pass or fail thresholds without a
  tolerance.
