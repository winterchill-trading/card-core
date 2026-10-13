(** Strong customer authentication for one card payment (PSD2 art. 97, RTS 2018/389),
    decided by the issuer when the authorization arrives.

    A payment is authenticated when 3-D Secure returned [Y] or the terminal verified the
    cardholder (online PIN, or a device's own verification for a phone wallet). Otherwise
    it needs an exemption the issuer accepts, or it is out of scope (merchant-initiated
    payments, mail and telephone orders); failing all that the answer is the soft decline
    [1A], which asks the merchant to authenticate and try again.

    Every payment let through without SCA names its exemption and RTS article, because
    the issuer then carries the loss if it was fraud (PSD2 art. 74(2)). The cumulative
    counters for the low-value exemptions come from payments-core's
    {!Payments_core.Sca.Counters}; they are per cardholder, and kept by the caller. *)

open! Core

module Initiator : sig
  type t =
    | Cardholder
    | Merchant (** a merchant-initiated payment (recurring, card on file) *)
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

module Policy : sig
  (** Which merchant-requested exemptions the issuer honours. Contactless, low value
      and recurring are always honoured within their limits. *)
  type t =
    { tra : bool
    (** transaction risk analysis (art. 18). Off until the issuer tracks its own
        rolling fraud rates against the RTS annex bands. *)
    ; trusted_beneficiary : bool (** art. 13: the merchant is on the payer's list *)
    ; corporate : bool (** art. 17: dedicated corporate processes *)
    }
  [@@deriving sexp, compare, equal]

  (** Everything off except the always-honoured exemptions. *)
  val default : t
end

module Request : sig
  type t =
    { entry_mode : Entry_mode.t
    ; initiator : Initiator.t
    ; amount : Money.Amount.t
    (** in the billing currency; the euro thresholds only apply to EUR amounts *)
    ; three_ds : Three_ds.t option
    ; exemption : Payments_core.Sca.Exemption.t option (** as flagged by the merchant *)
    ; cardholder_verified : bool (** online PIN at the terminal, or device verification *)
    }
  [@@deriving sexp_of]
end

module Out_of_scope : sig
  type t =
    | Merchant_initiated
    | Mail_or_telephone_order
  [@@deriving sexp, compare, equal, hash]
end

module Outcome : sig
  type t =
    | Authenticated
    | Exempt of Payments_core.Sca.Exemption.t
    | Out_of_scope of Out_of_scope.t
    | Required (** decline with 1A: authenticate and retry *)
    | Failed (** 3-D Secure said N or R: decline *)
  [@@deriving sexp_of, compare, equal]

  (** What the ledger keeps on the payment, as [Ledger.Sca_evidence]: an exemption is
      recorded with its RTS article (["Art 16 Low_value_remote"]), authentication as a
      [Challenge] named ["3ds"] or ["cardholder_verified"] (the card scheme, not us,
      holds the authentication record). [None] for [Required] and [Failed], which
      book nothing. *)
  val evidence : t -> Request.t -> Ledger.Sca_evidence.t option
end

val decide : Policy.t -> Payments_core.Sca.Counters.t -> Request.t -> Outcome.t

(** Counters after an approved payment: reset after authentication, increased after a
    contactless or low-value exemption, unchanged otherwise. *)
val counters_after
  :  Payments_core.Sca.Counters.t
  -> Request.t
  -> Outcome.t
  -> Payments_core.Sca.Counters.t
