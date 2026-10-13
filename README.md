# card-core

Card issuing in OxCaml, provider-neutral: the card lifecycle, card controls, the issuer's
authorization decision (including PSD2 strong customer authentication for card payments),
clearing and settlement against holds, disputes, statements, and the ledger entries for
each event. Amounts come from [`money`](https://github.com/winterchill-trading/money),
entries go to [`ledger`](https://github.com/winterchill-trading/ledger), and SCA
exemptions and counters are shared with
[`payments-core`](https://github.com/winterchill-trading/payments-core).

The library never sees a full card number, CVV or PIN, and it stores nothing: callers keep
cards, authorizations and clearings in their own store and pass them in.

## Why separate

A card payment does not behave like a bank transfer. It is approved in seconds against a
hold, cleared days later for a different amount (a tip, a hotel extra, a currency
conversion), cleared in parts or not at all, refunded weeks later as a new credit, and
sometimes disputed months later. Those rules are the same whichever issuer-processor sits
behind the cards, so they live here once, with properties that check the holds always
reconcile with the ledger; provider adapters only translate events into these types.

## Quick start

```
$ dune build
$ dune exec card -- codes
$ dune exec card -- hold-expiry -mcc 7011
30d
$ dune exec card -- demo
a1 COFFEE BAR 4.20 EUR -> 00 (Approve(amount"4.20 EUR")(partial false)(sca(Exempt Contactless)))
  book evt-a1/hold              CARD_AUTH        4.20 EUR
...
a3 ELECTRONICS 600.00 EUR -> 61 (Decline(Limit Per_transaction))
...
c2 Debit 46.00 EUR
  book evt-c2/settle            CARD_SETTLEMENT  46.00 EUR
...
```

No environment variables, no network, no credentials.

## Usage

An authorization request arrives (from the provider's real-time authorization webhook),
gets a decision, and the approved hold is booked; the clearing settles over it.

```ocaml
open! Core
open Card_core

let answer ~card ~authorizations ~balances (request : Authorization.Request.t) =
  let open Or_error.Let_syntax in
  let%bind usage =
    Authorization.usage authorizations ~currency:card.Card.currency ~now:request.at
  in
  let input : Decision.Input.t =
    { card
    ; usage
    ; available = Ledger.Balances.available balances card.account card.currency
    ; sca_policy = Card_sca.Policy.default
    ; sca_counters = Payments_core.Sca.Counters.zero (* the cardholder's, from your store *)
    ; request
    ; open_auth = None
    }
  in
  let decision = Decision.decide input ~now:request.at in
  let%bind auth = Authorization.pending request in
  let%bind auth = Decision.apply decision auth ~request in
  let%map entries =
    Booking.authorized balances card auth ~event_id:"provider-event-1" ~at:request.at
  in
  (* Reply with this code, store [auth], append [entries] to the journal. *)
  Decision.response_code decision, auth, entries
;;

let clear ~card ~auth ~balances (clearing : Settlement.Clearing.t) =
  let open Or_error.Let_syntax in
  let%bind outcome = Settlement.settle clearing ~auth:(Some auth) in
  Booking.settled Booking.Chart.default balances card clearing outcome
    ~event_id:"provider-event-2"
;;
```

Book the entries one event returns together, with `Ledger.Journal.append_linked` (or
`Payments_core.Booking.post`). A redelivered event builds entries under the same
idempotency keys, and the journal answers `Duplicate`.

## Modules

| Module | What it is |
|---|---|
| `Card` | id, holder (opaque id), funding account, billing currency, kind (Virtual, Physical), status, masked PAN, limits. Status machine below. |
| `Masked_pan`, `Pan_display_token`, `Pan_guard` | Last four digits and expiry only; an opaque, never-printed token for the provider's PAN display component; a Luhn check that refuses or masks card numbers in free text (merchant names, dispute notes). |
| `Limits` | Per transaction, daily, monthly, ATM amount and count caps; card-absent, contactless and ATM toggles; MCC and country block lists; `check` and `headroom`. |
| `Authorization` | The auth record: requested and approved amounts, merchant, MCC, country, entry mode, 3DS result, SCA evidence, response code, who decided, expiry, and the captured, cleared, released and refunded totals. Increments, partial reversals, expiry, clearings, refunds. `usage` for the limits. |
| `Decision` | Pure: card + limits + usage + available balance + SCA + risk hook -> `Approve { amount; partial; sca }` or `Decline reason`. |
| `Card_sca`, `Three_ds` | SCA for a card payment: authenticated by 3DS or the terminal, exempt (RTS art. 11 contactless, 12 transport and parking, 13 trusted beneficiary, 14 recurring, 16 low value, 17 corporate, 18 TRA), out of scope (merchant-initiated, mail or telephone order), or the `1A` soft decline. ECI per scheme and liability shift. |
| `Settlement` | Clearing records (debit and credit, FX with the scheme rate and our fee) and how each one settles against its authorization. |
| `Booking` | Ledger entries for every event (table below), pure over `Ledger.Balances.t`. |
| `Dispute` | Reason codes in four categories, stages with their time limits, evidence, outcome, and the 3DS liability check for fraud disputes. |
| `Statement` | One card's purchases, refunds, FX fees, pending holds and (optionally) declines for a period. |
| `Response_code`, `Decline_reason` | ISO 8583 field 39 codes with retry categories; why we declined and which code that sends. |
| `Mcc`, `Country`, `Entry_mode`, `Merchant`, `Ids` | The small types the rest is built from. |

### Card states

| From | To |
|---|---|
| Requested | Active, Destroyed |
| Active | Frozen, Blocked, Expired, Destroyed |
| Frozen | Active, Blocked, Expired, Destroyed |
| Blocked | Destroyed |
| Expired | Destroyed |
| Destroyed | (terminal) |

Every state but Active declines: Requested and Frozen with `57`, Blocked `62`, Expired
`54`, Destroyed `46`. (`Requested` is the research notes' `Inactive`; Blocked, Expired and
Destroyed split their `Closed` so the decline code says why.)

### Decision order

Card (14, 57, 62, 54, 46), billing currency (05), incremental auth has an open
authorization (05), limits and controls (57, 61, 65), funds (51, or a partial approval
when the merchant supports it), SCA (`1A`, or 05 when 3DS failed), risk hook (Review and
Block both 05, so a risk decline looks like any other). Funds come before SCA so nobody is
challenged for a payment that would fail anyway. Advices (stand-in, provider defaults) are
recorded with `Authorization.advice`, never decided.

### Ledger entries

| Event | Entries (code) |
|---|---|
| Approved authorization | hold of the approved amount on the funding account, keyed `card-auth:<auth id>`, expiring with the auth (`CARD_AUTH`) |
| Incremental authorization | the hold grows; the expiry does not move (`CARD_AUTH`) |
| Reversal, full or partial | that much of the hold released (`REVERSAL`) |
| Expiry (7 days; 30 for lodging, car rental, cruise and estimated amounts) | the rest of the hold released (`HOLD`) |
| Clearing within the hold | capture to `clearing:card/<cur>`; a final clearing releases the rest in the same entry (`CARD_SETTLEMENT`) |
| Clearing above the hold (tip, hotel extra) | one over-capture entry; flagged for review above 20% of the approved amount |
| Clearing after the hold lapsed, or without an authorization | a new debit; what the account cannot cover goes to `receivable:card/<cur>` so the customer account never goes negative |
| FX fee | its own entry to `income:fees/<cur>` (`FEE`) |
| Refund (credit clearing) | a new credit from `clearing:card/<cur>` (`REFUND`), never a reversal |
| Chargeback won | the disputed amount credited the same way (`REFUND`) |

Idempotency keys are `<provider event id>/<leg>`; references are the auth, clearing or
dispute id.

## Serialization

`Card`, `Authorization`, `Settlement.Clearing`, `Dispute`, `Limits`, `Merchant`,
`Masked_pan`, `Three_ds`, `Decline_reason` and `Response_code` have `Stable.V1` Bin_prot
and sexp forms with their shape digests pinned in `test/test_stable.ml`; a change to any of
them fails the build. Decoding validates again where a value has rules (four-digit last
four, PAN-free merchant names, limit currencies). Times print in UTC.

## Verified

Expect tests for the standard flows (approve, decline for insufficient funds, decline on a
frozen, blocked, requested or expired card, partial approval, split-shipment partial
settlement, partial and full reversal, refund, tip over-capture, over-capture the account
cannot pay, expiry then a late clearing, a clearing after the hold lapsed, FX with our fee,
a redelivered webhook against a real journal), SCA outcomes and counters, disputes,
statements, and a cram test of the CLI.

Quickcheck properties:

- holds and captures reconcile: after any sequence of approvals, increments, reversals,
  clearings (partial, final, above the hold, without an auth), refunds and expiries, each
  authorization's open hold equals its ledger hold, the account's held funds equal the sum
  of open holds, money is conserved (account plus receivable equals deposit minus
  clearings plus refunds), the scheme position matches, and the trial balance is zero;
- limits are never exceeded by approved authorizations in a day or a month, per
  transaction, for ATM amount and for ATM count, whatever the order and size of requests;
- card transitions only follow the table; a changed digit always breaks the Luhn check;
  every exempt payment records its RTS article.

Both main properties were checked against mutants (a disabled daily cap, a capture that
never releases the remainder) and fail on them. There is no external API, so nothing is
verified live.

## Known gaps

- The ledger's hold builders take no SCA evidence, so the 3DS or exemption record is kept
  on the `Authorization` and on direct debits only, not on the hold and capture entries.
- Hold validity is 7 or 30 days, a simplification of the scheme tables (Visa uses 5 days
  for card-present and merchant-initiated, 10 for card-absent).
- Usage windows are UTC days and calendar months, not the cardholder's time zone.
- Partial approvals cover insufficient funds only, not amount caps.
- The SCA counters (`Payments_core.Sca.Counters`) count euro amounts only, so the
  contactless and low-value exemptions never apply to cards billed in other currencies.
- A receivable left by a force-posted clearing is not netted against later deposits; the
  service layer decides how to collect it.
- No network tokens (device or card-on-file token lifecycles) and no interchange; those
  belong to the provider adapter until a provider exposes them.
- Disputes use one 120-day filing window for every reason.
