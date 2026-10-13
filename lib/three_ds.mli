(** The 3-D Secure 2 result an authorization carries: the ACS's [transStatus] and the
    Electronic Commerce Indicator (ECI) the merchant puts into the authorization.

    An authenticated result (ECI 05 at Visa, 02 at Mastercard) or an attempted one (06 or
    01) moves liability for card-absent fraud disputes to the issuer; ECI 07 (Visa) or 00
    (Mastercard) means no shift. *)

open! Core

module Scheme : sig
  type t =
    | Visa
    | Mastercard
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]
end

module Trans_status : sig
  type t =
    | Y (** authenticated, frictionless or after a challenge *)
    | A (** attempted: the ACS was not available, a proof of attempt was issued *)
    | I (** informational only: the issuer accepted an exemption (3DS 2.2) *)
    | N (** not authenticated: deny *)
    | U (** could not be performed *)
    | R (** rejected by the issuer: do not authorize *)
    | C (** challenge required (interim) *)
    | D (** decoupled challenge (interim, 3DS 2.2) *)
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

  val of_string_or_error : string -> t Or_error.t
  val to_string : t -> string

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

type t =
  { trans_status : Trans_status.t
  ; eci : int option (** as in the authorization, e.g. 5 for ["05"] *)
  ; cavv_present : bool (** the cryptogram proving the authentication was sent *)
  }
[@@deriving sexp, compare, equal, hash, quickcheck]

(** The ECI a merchant should send for this status: Visa 05/06/07, Mastercard 02/01/00;
    [None] for the interim and rejected statuses. *)
val expected_eci : Scheme.t -> Trans_status.t -> int option

(** Whether a card-absent fraud dispute is the issuer's loss: an authenticated or
    attempted ECI with the cryptogram present. *)
val liability_shift : Scheme.t -> t -> bool

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
