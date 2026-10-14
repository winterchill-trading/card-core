# AGENTS.md (bin/)

`card` executable: `codes` (response codes), `hold-expiry -mcc MCC` (hold validity for an MCC) and
`demo` (an offline authorization and clearing walk-through). The cram test pins the output of the
CLI through `test/`.

- Build: `dune build --root .. @card-core/all` from the repository root.
