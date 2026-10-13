(** ISO 3166-1 alpha-2 country codes (["DE"], ["FR"]), checked for shape only: two
    upper-case letters. *)

open! Core

type t [@@deriving sexp, compare, equal, hash, quickcheck]

include Comparable.S with type t := t

val of_string_or_error : string -> t Or_error.t
val of_string : string -> t
val to_string : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
