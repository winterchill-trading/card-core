(** Ledger entries for card events. Pure: every function takes the current
    {!Ledger.Balances.t} and returns the entries to book, already checked against it, so
    the caller appends them to its journal in order (or drops them). Nothing is persisted
    here.

    - Authorization approved: a hold of the approved amount on the card's funding account,
      keyed by the authorization id and expiring with it ([Card_auth]).
    - Incremental authorization: the hold grows; the expiry does not move.
    - Reversal: the hold shrinks by the reversed amount (all of it by default).
    - Expiry: what is left of the hold goes back.
    - Debit clearing against a hold: one entry captures [min amount hold] to the card
      settlement account and, on the final clearing, releases the rest. A clearing above
      the hold is one over-capture entry when the account can pay the excess.
    - A debit the account cannot pay (an over-capture, a late clearing after the hold
      expired, a clearing with no authorization; the scheme does not ask before it clears)
      is still booked: the shortfall goes to a card receivable, a claim on the cardholder,
      so the customer account never goes below zero.
    - FX fee: its own entry to fee income, so it shows as its own statement line.
    - Refund (credit clearing): a new credit from the card settlement account, never a
      reversal of an earlier entry.
    - Chargeback won: the disputed amount comes back the same way, as a credit.

    Every entry has the authorization or clearing id as [reference] and [<event id>/<leg>]
    as its idempotency key, where the event id is the provider event that caused it, so a
    webhook delivered twice is booked once. *)

open! Core

module Chart : sig
  type t =
    { card_settlement : Money.Currency.t -> Ledger.Account_id.t
    (** our position with the card scheme for one currency: a clearing account that may go
        negative *)
    ; card_receivable : Money.Currency.t -> Ledger.Account_id.t
    (** what cardholders owe after a debit their account could not cover *)
    ; fee_income : Money.Currency.t -> Ledger.Account_id.t
    }

  (** [clearing:card/<currency>], [receivable:card/<currency>] and
      [income:fees/<currency>]. *)
  val default : t

  (** The accounts the chart names for these currencies, ready to open. *)
  val accounts : t -> currencies:Money.Currency.t list -> Ledger.Account.t list
end

(** [card-auth:<auth id>]. Fails when the id has characters a ledger id does not allow. *)
val hold_id : Ids.Auth_id.t -> Ledger.Hold_id.t Or_error.t

(** The hold for a newly approved authorization (also an approved advice). Nothing for a
    declined one. *)
val authorized
  :  Ledger.Balances.t
  -> Card.t
  -> Authorization.t
  -> event_id:string
  -> at:Time_ns.t
  -> Ledger.Entry.t list Or_error.t

val incremented
  :  Ledger.Balances.t
  -> Authorization.t
  -> amount:Money.Amount.t
  -> event_id:string
  -> at:Time_ns.t
  -> Ledger.Entry.t list Or_error.t

val reversed
  :  Ledger.Balances.t
  -> Authorization.t
  -> amount:Money.Amount.t
  -> event_id:string
  -> at:Time_ns.t
  -> Ledger.Entry.t list Or_error.t

val expired
  :  Ledger.Balances.t
  -> Authorization.t
  -> event_id:string
  -> at:Time_ns.t
  -> Ledger.Entry.t list Or_error.t

(** Entries for a clearing whose {!Settlement.settle} outcome is given. If the ledger hold
    has already passed its expiry, it is expired first and the clearing books as fresh
    spend. *)
val settled
  :  Chart.t
  -> Ledger.Balances.t
  -> Card.t
  -> Settlement.Clearing.t
  -> Settlement.Outcome.t
  -> event_id:string
  -> Ledger.Entry.t list Or_error.t

(** A chargeback decided in the cardholder's favour: [amount] credited back. *)
val chargeback_won
  :  Chart.t
  -> Ledger.Balances.t
  -> Card.t
  -> dispute_id:Ids.Dispute_id.t
  -> amount:Money.Amount.t
  -> event_id:string
  -> at:Time_ns.t
  -> Ledger.Entry.t list Or_error.t
