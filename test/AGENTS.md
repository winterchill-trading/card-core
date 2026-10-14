# AGENTS.md (test/)

Tests for `card_core`.

- Expect tests cover approval, insufficient funds, a frozen card, partial settlement, reversal,
  refund, a tip above the hold, a late clearing after expiry and FX with a fee.
- `test_properties.ml` holds Base_quickcheck properties: holds reconcile with the ledger, and
  approved authorizations never exceed limits. Both were checked against deliberately broken code.
- `test_stable.ml` pins the digests of the stable types.
- The cram test of the CLI is in `test/dune` (`(cram (deps %{bin:card}))`); promote its output with
  `dune promote` only after reading the diff.
- Run: `dune build --root .. @card-core/runtest` from the repository root.
