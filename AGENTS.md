# AGENTS.md

## Purpose

Provider-neutral card issuing: the card lifecycle and controls, the issuer's authorization
decision (with PSD2 strong customer authentication and 3-D Secure), clearing and settlement
against holds with FX, disputes, per-card statements, and the ledger entries for each event. The
library persists nothing and never sees a full card number, CVV or PIN.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | Library `card_core`: `Card`, `Authorization`, `Decision`, `Card_sca`, `Three_ds`, `Settlement`, `Booking`, `Dispute`, `Statement`, `Limits`, and the small id and code types (see `lib/AGENTS.md`) |
| `bin/` | `card` executable: `codes`, `hold-expiry`, `demo` (see `bin/AGENTS.md`) |
| `test/` | Expect tests for flows and decisions, Quickcheck properties, stable digests, cram tests of the CLI (see `test/AGENTS.md`) |
| `.github/` | CI workflow (see `.github/AGENTS.md`) |
| `dune-project`, `card-core.opam` | Package metadata; the `.opam` file is generated from `dune-project` |

## Build, test, docs

Run from this directory. The workspace root `..` holds the sibling checkouts and the shared
`dune-workspace`.

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @card-core/all
opam exec --switch 5.2.0+ox -- dune build --root .. @card-core/runtest
DUNE_BUILD_DIR=_build-card-core opam exec --switch 5.2.0+ox -- dune build --root .. @card-core/all
```

The last line is for parallel work: a private build directory avoids the lock shared with
other repositories in the workspace.

Try the CLI from the repository root: `dune exec card -- codes`,
`dune exec card -- hold-expiry -mcc 7011`, `dune exec card -- demo`.

Standalone (outside the workspace), pin `fixed-decimal`, `money`, `ledger` and `payments-core`
from GitHub as the CI workflow does, then `opam install . --deps-only --with-test` and
`dune build && dune test`.

## Conventions

- Formatting: `dune build --root .. @card-core/fmt --auto-promote`, run after the tests pass.
  Never `dune fmt` at the workspace root; it reformats every sibling repository.
- Every module has an `.mli`. Use `open! Core`, labelled arguments and `ppx_jane`.
- Decisions are pure. `Decision.decide` takes the input, an optional risk hook and `~now`, and returns
  `Approve` or `Decline`; the check order is fixed (funds before SCA). Changing the order changes responses.
- Ledger entries are built as pure functions over `Ledger.Balances.t` in `Booking`; the caller posts them.
- Never add a field that carries a full PAN, CVV or PIN. Free text goes through `Pan_guard`.
- Types that cross a boundary are `Stable.V1` with pinned digests (`test/test_stable.ml`).
- Add a short comment at any non-obvious choice, such as an RTS exemption or a hold rule.
- Document every public value in its `.mli` with odoc comments.

## Dependencies and dependents

- Depends on: `money`, `ledger`, `payments-core` (SCA types, exemptions and counters).
- Used by: `banking-provider` (the fake provider's card booking).
- CI pin order: `fixed-decimal`, `money`, `ledger`, `payments-core`.

## CI

`.github/workflows/ci.yml` runs the `ox-setup` action with the pins above (its opam switch is
cached between runs), then `dune build` and `dune test`. The `obj-magic` job calls the shared
reusable workflow. To check a run: `gh run list --limit 1`, then at most one
`gh run watch <id> --exit-status`. Do not poll in a loop.

## Pitfalls

- Booking functions check their entries against `Balances` with the time you pass. The journal
  stamps records at least 1 ns apart, so a synthetic clock that stands still fails on the second
  booking. Pass `Ledger.Journal.now journal`.
- `Booking.settled` takes its time from the clearing's `at`, so build the clearing with the
  journal's time too.
- Do not `open Card_core` or any other growing sibling; alias the modules you use. A shadowed
  name broke this build twice.
- Hold builders take no SCA evidence yet; that is a `ledger` follow-up.
- A Quickcheck shrinker in a submodule, `let quickcheck_shrinker = Quickcheck.Shrinker.empty ()`,
  needs an annotation (`: t Quickcheck.Shrinker.t`) or it fails to type.
- To mutation-check a property, disable one rule and add `ignore x;` for any variable the mutant
  leaves unused; otherwise warning 26 fails the dev build before a test runs.
- A `Stable.V1` type cannot `include Int` in a module that defines its own `Stable`; include
  `Identifiable.S` with the type equality instead.
- `dune promote` fills cram output for `card` and `hold-expiry`. Read the diff first.

## Where to read more

- `README.md` (usage, decision order, ledger entries, known gaps).
- `../specs/repos/card-core.md` for status, known gaps and dependencies.
- Skills in `../oxcaml-claude-skills/skills/`: `banking-standards`, `janestreet-ocaml-style`.
- `../oxcaml-claude-skills/AGENTS.md`: the workspace-wide guide.
- `../oxcaml-claude-skills/GOTCHAS.md`: the dated log these pitfalls come from.

## Never do

- Run `opam install`, `opam upgrade` or `opam pin` on the shared `5.2.0+ox` switch without a
  dry run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`. Abort if the plan touches
  `ocaml-variants`, `core`, `base`, `async` or `ppx_jane`.
- Publish, tag or release a version from this repository without the maintainers asking.
- Commit card numbers or credentials, even in tests.
- Poll CI in a loop.
