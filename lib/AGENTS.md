# AGENTS.md (lib/)

Library `card_core`: the card domain, with no I/O and no storage.

- `Card` and `Authorization` hold the state machines (card: Requested, Active, Frozen, Blocked,
  Expired, Destroyed; authorization: Pending to Cleared, or Declined, Reversed, Expired).
- `Decision.decide` is the authorization decision; `Settlement` and `Booking` turn clearings into
  ledger entries; `Dispute` and `Statement` are views over the same records.
- `Pan_guard` refuses or masks card numbers in free text. Route any user-visible string through it.
- Build: `dune build --root .. @card-core/all` from the repository root.
