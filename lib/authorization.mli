(** One card authorization and what happened to it afterwards.

    An authorization is a hold, not a debit: the approved amount is reserved on the
    funding account until clearings take it, a reversal gives some or all of it back, or
    it expires. States:

    {v
      Pending --> Authorized --> Partially_cleared --> Cleared
         |            |  \                 |
         v            |   +--> Reversed    +--> (rest released: Cleared)
      Declined        +------> Expired
    v}

    [Authorized] is the "approved" state. Advices (the scheme or the provider already
    approved, e.g. stand-in when we did not answer in time) enter [Authorized] directly
    and cannot be declined. An incremental authorization adds to an open one under the
    same network transaction id and does not extend its validity. Reversals may be
    partial. A clearing may take part of the hold (several clearings per authorization
    are allowed), all of it, more than it (tips, hotel extras: the excess is new spend),
    or arrive after the hold was released (it still posts). Refunds are separate credits
    and never reopen the hold.

    All amounts except [merchant.amount] are in the card's billing currency. *)

open! Core

module Kind : sig
  type t =
    | Auth (** 0100: dual message, clearing follows *)
    | Incremental (** adds to an approved estimate *)
    | Advice (** 0120: decided already, recorded only *)
    | Financial (** 0200: single message (ATM, PIN debit), hold and clear in one go *)
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]
end

module Decided_by : sig
  type t =
    | Us
    | Stand_in (** the scheme answered because we did not in time *)
    | Provider_default (** the issuer-processor's configured default after its timeout *)
  [@@deriving sexp, compare, equal, hash, enumerate]
end

module Status : sig
  type t =
    | Pending
    | Authorized
    | Partially_cleared
    | Cleared
    | Declined of Decline_reason.t
    | Reversed
    | Expired
  [@@deriving sexp, compare, equal]

  (** [Authorized] or [Partially_cleared]: a hold may remain. *)
  val is_open : t -> bool
end

(** An authorization request as it arrives from the provider. *)
module Request : sig
  type t =
    { auth_id : Ids.Auth_id.t
    ; card_id : Ids.Card_id.t
    ; network_txn_id : Ids.Network_txn_id.t
    ; kind : Kind.t
    ; amount : Money.Amount.t (** billing currency *)
    ; merchant : Merchant.t
    ; entry_mode : Entry_mode.t
    ; initiator : Card_sca.Initiator.t
    ; estimated : bool (** an estimated amount (hotel, car rental, fuel) *)
    ; partial_supported : bool (** the merchant accepts a partial approval *)
    ; three_ds : Three_ds.t option
    ; exemption : Payments_core.Sca.Exemption.t option
    ; cardholder_verified : bool
    ; at : Payments_core.Utc_time.t
    }
  [@@deriving sexp_of]

  val sca_request : t -> Card_sca.Request.t
end

type t = private
  { id : Ids.Auth_id.t
  ; card_id : Ids.Card_id.t
  ; network_txn_id : Ids.Network_txn_id.t
  ; kind : Kind.t (** of the first message; never [Incremental] *)
  ; requested : Money.Amount.t (** increments included *)
  ; approved : Money.Amount.t (** zero when declined; increments included *)
  ; merchant : Merchant.t
  ; entry_mode : Entry_mode.t
  ; initiator : Card_sca.Initiator.t
  ; estimated : bool
  ; three_ds : Three_ds.t option
  ; sca : Ledger.Sca_evidence.t option
  ; response_code : Response_code.t option (** [None] while pending *)
  ; decided_by : Decided_by.t
  ; status : Status.t
  ; increments : Ids.Auth_id.t list (** ids of the incremental messages, oldest first *)
  ; captured : Money.Amount.t (** taken from the hold by clearings *)
  ; cleared : Money.Amount.t (** all clearings, including amounts above the hold *)
  ; released : Money.Amount.t (** given back by reversals and expiry *)
  ; refunded : Money.Amount.t
  ; created_at : Payments_core.Utc_time.t
  ; expires_at : Payments_core.Utc_time.t
  ; updated_at : Payments_core.Utc_time.t
  }
[@@deriving sexp_of, compare, equal]

(** 7 days; 30 days for an estimated authorization or a lodging, vehicle rental or
    cruise merchant. A simplification of the scheme tables (Visa allows 5 to 30 days by
    case, Mastercard 7 or 30) that never releases earlier than the merchant expects for
    the common cases. *)
val hold_validity : mcc:Mcc.t -> estimated:bool -> Time_ns.Span.t

(** {2 Deciding} *)

(** A new [Pending] authorization; fails for an [Incremental] request (see
    {!increment}) or an amount that is not positive. *)
val pending : Request.t -> t Or_error.t

(** [Pending -> Authorized]; [amount] is at most [requested] (less for a partial
    approval). *)
val approve
  :  t
  -> amount:Money.Amount.t
  -> sca:Ledger.Sca_evidence.t option
  -> decided_by:Decided_by.t
  -> at:Time_ns.t
  -> t Or_error.t

(** [Pending -> Declined]. *)
val decline : t -> Decline_reason.t -> decided_by:Decided_by.t -> at:Time_ns.t -> t Or_error.t

(** An advice: recorded as decided upstream, [Authorized] for an approval (code 00, for
    [approved]) or [Declined (Declined_upstream code)] otherwise. *)
val advice
  :  Request.t
  -> approved:Money.Amount.t
  -> response_code:Response_code.t
  -> decided_by:Decided_by.t
  -> t Or_error.t

(** Adds an approved incremental authorization ([0 < approved <= request.amount]) to an
    open one with the same card and network transaction id. The expiry is unchanged. *)
val increment : t -> Request.t -> approved:Money.Amount.t -> t Or_error.t

(** {2 After approval} *)

(** What is still reserved: [approved - captured - released] while open, else zero. *)
val open_hold : t -> Money.Amount.t

(** What counts against the card's limits: [cleared + open_hold]. Reversed and expired
    amounts do not count; refunds do not give limit back. *)
val spend : t -> Money.Amount.t

(** Releases [amount] (default: all of the open hold); the rest stays open. Fails above
    the open hold or on a closed authorization. Returns the amount released. *)
val reverse : t -> ?amount:Money.Amount.t -> at:Time_ns.t -> unit -> (t * Money.Amount.t) Or_error.t

val is_due_to_expire : t -> now:Time_ns.t -> bool

(** Releases the open hold once [now] reaches [expires_at]. *)
val expire : t -> now:Time_ns.t -> (t * Money.Amount.t) Or_error.t

(** How one clearing splits against the hold. *)
module Applied : sig
  type t =
    { from_hold : Money.Amount.t (** [min amount open_hold] *)
    ; excess : Money.Amount.t (** above the hold: new spend *)
    ; released : Money.Amount.t (** rest of the hold given back by a final clearing *)
    }
  [@@deriving sexp_of, compare, equal]
end

(** Applies a debit clearing of [amount]. [final] marks the merchant's last clearing for
    this authorization (single clearing, or the last of a split shipment) and releases
    what is left of the hold. A clearing after the hold was reversed or expired takes
    nothing from it and is all excess. Fails for a pending or declined authorization:
    such a clearing is booked as one without an authorization. *)
val apply_clearing
  :  t
  -> amount:Money.Amount.t
  -> final:bool
  -> at:Time_ns.t
  -> (t * Applied.t) Or_error.t

(** Records a refund linked to this authorization. *)
val record_refund : t -> amount:Money.Amount.t -> at:Time_ns.t -> t Or_error.t

(** Spend of the card's authorizations created on the UTC date of [now] and in its UTC
    calendar month, in [currency]; ATM withdrawals counted when their spend is
    positive. *)
val usage : t list -> currency:Money.Currency.t -> now:Time_ns.t -> Limits.Usage.t Or_error.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
