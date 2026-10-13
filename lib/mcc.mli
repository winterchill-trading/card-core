(** Merchant category codes (ISO 18245): four digits saying what kind of business a
    merchant is. Card controls and hold expiry depend on a few of them. *)

open! Core

type t [@@deriving sexp, compare, equal, hash, quickcheck]

include Comparable.S with type t := t

(** Fails outside [0000, 9999]. *)
val of_int : int -> t Or_error.t

val of_int_exn : int -> t
val to_int : t -> int

(** Four digits with leading zeros: ["0742"]. *)
val to_string : t -> string

val of_string_or_error : string -> t Or_error.t

(** {2 Codes with special handling} *)

(** 6011 *)
val atm : t

(** 6051 *)
val quasi_cash : t

(** 4829 *)
val money_transfer : t

(** 6540 *)
val stored_value : t

(** 7995 *)
val gambling : t

(** 7011 *)
val lodging : t

(** 7512 *)
val vehicle_rental : t

(** 4411 *)
val cruise : t

(** 5542, automated fuel dispenser *)
val fuel : t

val is_atm : t -> bool

(** Lodging, vehicle rental and cruise lines: their estimated authorizations stay valid
    for 30 days instead of 7. *)
val has_long_hold : t -> bool

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
