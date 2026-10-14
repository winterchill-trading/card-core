# AGENTS.md (.github/)

`workflows/ci.yml` builds and tests this repository with the `ox-setup` action. Its `pins:` block
lists the sibling repositories in dependency order; keep it in sync with `dune-project`. The
`obj-magic` job calls the shared reusable workflow from `infra`.
