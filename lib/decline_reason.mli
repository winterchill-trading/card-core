(** Why an authorization was declined, and the response code that goes back to the
    scheme. Reasons name the rule; the cardholder-facing text is the caller's job and
    must not reveal risk rules (a risk decline reads as a generic decline). *)

open! Core

type t =
  | Unknown_card (** 14 *)
  | Card_status of Card.Status.t (** see {!Card.Status.decline_code} *)
  | Card_expired (** 54: past the expiry date printed on the card *)
  | Wrong_currency (** 05: the billing amount is not in the card's currency *)
  | Limit of Limits.Violation.t (** 61, 65 or 57 *)
  | Insufficient_funds (** 51 *)
  | Sca_required (** 1A: authenticate and retry *)
  | Authentication_failed (** 05: 3-D Secure said N or R *)
  | Risk_review of string (** 05: held for review, declined meanwhile *)
  | Risk_block of string (** 05 *)
  | Unmatched_increment (** 05: an incremental authorization for no open authorization *)
[@@deriving sexp, compare, equal]

val response_code : t -> Response_code.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
