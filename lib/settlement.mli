(** Clearing (presentment) of card transactions and how each one settles against its
    authorization.

    A debit clearing carries the final amount, which may differ from the authorization:
    a tip, a fuel or hotel estimate, a split shipment cleared in parts, or a conversion
    when the merchant charges in another currency than the card's. Each one takes
    [min amount open_hold] from the hold; anything above is fresh spend, flagged for
    review when it exceeds {!excess_review_bps} of the approved amount. A clearing with
    no authorization (offline, under the floor limit, or one that already expired) posts
    in full. A credit clearing is a refund: a new credit to the cardholder, never a
    reversal of the hold.

    When the merchant currency differs from the billing currency, the billing amount is
    the scheme's conversion (given, or computed with [rate]), and our FX fee, if any, is
    charged on top as its own amount. *)

open! Core

module Direction : sig
  type t =
    | Debit
    | Credit (** a refund *)
  [@@deriving sexp, compare, equal, hash, enumerate]
end

module Fx : sig
  type t =
    { rate : Money.Rate.t (** merchant currency to billing currency *)
    ; fee : Money.Amount.t (** ours, in the billing currency; zero when none *)
    }
  [@@deriving sexp, compare, equal]
end

module Clearing : sig
  type t = private
    { id : Ids.Clearing_id.t
    ; card_id : Ids.Card_id.t
    ; network_txn_id : Ids.Network_txn_id.t option
    ; auth_id : Ids.Auth_id.t option (** the authorization it was matched to *)
    ; direction : Direction.t
    ; merchant : Merchant.t (** with the amount in the merchant currency *)
    ; billing : Money.Amount.t (** what the cardholder is charged or refunded *)
    ; fx : Fx.t option
    ; final : bool (** the last clearing for this authorization *)
    ; at : Payments_core.Utc_time.t
    }
  [@@deriving sexp_of, compare, equal]

  (** Without [billing], the billing amount is [merchant.amount] when the currencies
      match, else [Rate.convert rate merchant.amount] (half-even). [rate] must go from
      the merchant currency to [billing_currency]. [fx_fee] (a {!Money.Fee.t} in the
      billing currency, rounded in our favour) applies only to a debit in another
      currency. *)
  val create
    :  id:Ids.Clearing_id.t
    -> card_id:Ids.Card_id.t
    -> ?network_txn_id:Ids.Network_txn_id.t
    -> ?auth_id:Ids.Auth_id.t
    -> direction:Direction.t
    -> merchant:Merchant.t
    -> billing_currency:Money.Currency.t
    -> ?billing:Money.Amount.t
    -> ?rate:Money.Rate.t
    -> ?fx_fee:Money.Fee.t
    -> ?final:bool (** default [true] *)
    -> at:Time_ns.t
    -> unit
    -> t Or_error.t

  (** [billing] plus the FX fee: the total debited for a debit clearing. *)
  val total_charged : t -> Money.Amount.t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

(** 2000 basis points: a clearing more than 20% above its authorization (tips and hotel
    extras included) is flagged for the risk rules. Our threshold, not a scheme rule. *)
val excess_review_bps : int

module Outcome : sig
  type t =
    | Against_auth of
        { auth : Authorization.t (** updated *)
        ; applied : Authorization.Applied.t
        ; review_excess : bool
        }
    | Without_auth (** posts the whole billing amount as new spend *)
    | Refund of { auth : Authorization.t option (** updated when linked *) }
  [@@deriving sexp_of]
end

(** Applies a clearing to its authorization, if any. An authorization for another card,
    or one that is pending or declined, is ignored and the clearing posts without
    one. *)
val settle : Clearing.t -> auth:Authorization.t option -> Outcome.t Or_error.t
