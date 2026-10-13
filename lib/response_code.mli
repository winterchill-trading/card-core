(** The two-character answer to an authorization (ISO 8583 field 39), and how a merchant
    may react to it.

    Declines are grouped as the card scheme groups them: category 1 must never be
    retried, categories 2 to 4 may be retried (up to 20 times in 30 days at one scheme).
    [Sca_required] (["1A"]) is the soft decline that asks the merchant to run strong
    customer authentication and try again. *)

open! Core

type t =
  | Approved (** 00 *)
  | Do_not_honor (** 05: generic, also used for a risk review *)
  | Invalid_account (** 14: no such card *)
  | Closed_account (** 46 *)
  | Insufficient_funds (** 51 *)
  | Expired_card (** 54 *)
  | Incorrect_pin (** 55 *)
  | Not_permitted (** 57: transaction not permitted to cardholder (frozen, control off) *)
  | Exceeds_amount_limit (** 61 *)
  | Restricted_card (** 62 *)
  | Exceeds_frequency_limit (** 65 *)
  | Fraud_security (** 83: scheme use only, listed so incoming advices parse *)
  | Issuer_unavailable (** 91 *)
  | Sca_required (** 1A *)
  | Other of string (** any other two-character code, kept as received *)
[@@deriving sexp, compare, equal, hash]

(** ["00"], ["51"], ["1A"]. *)
val to_string : t -> string

(** Accepts any two characters from [0-9A-Z]; codes not listed above become [Other]. *)
val of_string_or_error : string -> t Or_error.t

val is_approved : t -> bool

module Category : sig
  type t =
    | Never_approve (** 1: the merchant must not retry *)
    | Cannot_approve_now (** 2: may succeed later (funds, limits, issuer down) *)
    | Data_quality (** 3: fix the data (expiry, PIN) and retry *)
    | Generic (** 4: everything else *)
  [@@deriving sexp, compare, equal, enumerate]
end

(** [None] for [Approved]. *)
val category : t -> Category.t option

(** Whether the merchant may send the same transaction again. *)
val retry_allowed : t -> bool

(** Every listed code except [Other], for tables and tests. *)
val all_known : t list

module Stable : sig
  module V1 : sig
    (** Bin_prot and sexp are the two-character text. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
