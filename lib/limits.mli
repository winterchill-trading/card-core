(** Spending controls on one card, in the card's billing currency.

    Amount caps (per transaction, per UTC day, per UTC calendar month, ATM cash per day),
    an ATM withdrawal count per day, channel toggles (card-absent, contactless, ATM) and
    block lists of merchant categories and countries. [None] means no cap.

    {!check} answers whether one more transaction fits, given what the card has already
    spent ({!Usage}); it is one input to {!Decision}. *)

open! Core

module Usage : sig
  (** Approved spend that counts against the caps: authorizations still open or
      cleared, net of reversals. Built by {!Authorization.usage}. *)
  type t =
    { daily : Money.Amount.t
    ; monthly : Money.Amount.t
    ; atm_daily : Money.Amount.t
    ; atm_daily_count : int
    }
  [@@deriving sexp, compare, equal]

  val zero : Money.Currency.t -> t

  (** Adds one approved transaction; [atm] also counts it as a withdrawal. *)
  val add : t -> Money.Amount.t -> atm:bool -> t Or_error.t
end

module Violation : sig
  type t =
    | Per_transaction
    | Daily
    | Monthly
    | Atm_amount
    | Atm_count
    | Atm_disabled
    | Online_disabled
    | Contactless_disabled
    | Mcc_blocked of Mcc.t
    | Country_blocked of Country.t
  [@@deriving sexp, compare, equal]

  (** 61 for amount caps, 65 for the ATM count, 57 for toggles and block lists. *)
  val response_code : t -> Response_code.t
end

type t = private
  { currency : Money.Currency.t
  ; per_transaction : Money.Amount.t option
  ; daily : Money.Amount.t option
  ; monthly : Money.Amount.t option
  ; atm_daily : Money.Amount.t option
  ; atm_daily_count : int option
  ; online : bool (** card-absent payments allowed *)
  ; contactless : bool
  ; atm : bool
  ; blocked_mccs : Mcc.Set.t
  ; blocked_countries : Country.Set.t
  }
[@@deriving sexp_of, compare, equal]

(** Every cap in [currency] and positive, the count positive. Toggles default to on,
    block lists to empty. *)
val create
  :  ?per_transaction:Money.Amount.t
  -> ?daily:Money.Amount.t
  -> ?monthly:Money.Amount.t
  -> ?atm_daily:Money.Amount.t
  -> ?atm_daily_count:int
  -> ?online:bool
  -> ?contactless:bool
  -> ?atm:bool
  -> ?blocked_mccs:Mcc.t list
  -> ?blocked_countries:Country.t list
  -> Money.Currency.t
  -> t Or_error.t

(** No caps, every channel on. *)
val unlimited : Money.Currency.t -> t

val with_online : t -> bool -> t
val with_contactless : t -> bool -> t
val with_atm : t -> bool -> t

(** The first rule the transaction breaks, checked in this order: channel toggles, MCC
    and country block lists, per-transaction cap, daily and monthly caps, ATM amount and
    count. [amount] is in the billing currency; a different currency is an error. *)
val check
  :  t
  -> usage:Usage.t
  -> amount:Money.Amount.t
  -> mcc:Mcc.t
  -> country:Country.t
  -> entry_mode:Entry_mode.t
  -> (unit, Violation.t) Result.t Or_error.t

(** The most a transaction at this merchant could still be approved for under the amount
    caps, [None] when nothing caps it. Used for partial approvals. *)
val headroom : t -> usage:Usage.t -> mcc:Mcc.t -> Money.Amount.t option Or_error.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
