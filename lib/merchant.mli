(** The merchant side of a card transaction, as the scheme reports it. *)

open! Core

type t = private
  { name : string (** descriptor, checked by {!Pan_guard}: never holds a card number *)
  ; mcc : Mcc.t
  ; country : Country.t
  ; amount : Money.Amount.t (** in the merchant's currency, before any conversion *)
  }
[@@deriving sexp_of, compare, equal]

(** Fails when [name] looks like it holds a card number or [amount] is negative. *)
val create
  :  name:string
  -> mcc:Mcc.t
  -> country:Country.t
  -> amount:Money.Amount.t
  -> t Or_error.t

val with_amount : t -> Money.Amount.t -> t Or_error.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
