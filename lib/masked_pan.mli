(** What this library knows about a card number: its last four digits and its expiry.

    The full PAN, the CVV and the PIN never enter this library: the issuer-processor holds
    them, and a cardholder sees them only through the provider's hosted UI component (see
    {!Pan_display_token}). Keeping them out keeps our services out of PCI DSS scope. *)

open! Core

module Last4 : sig
  (** Exactly four digits. A longer string is refused, so a full card number cannot be
      stored here by mistake. *)
  type t [@@deriving sexp, compare, equal, hash, quickcheck]

  val of_string_or_error : string -> t Or_error.t
  val of_string : string -> t
  val to_string : t -> string

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

module Expiry : sig
  (** Month and year printed on the card; valid through the last day of that month. *)
  type t [@@deriving sexp, compare, equal, hash, quickcheck]

  (** [month] 1 to 12, [year] four digits from 2000. *)
  val create : month:int -> year:int -> t Or_error.t

  val month : t -> int
  val year : t -> int

  (** ["10/29"]. *)
  val to_string : t -> string

  (** The first instant (UTC) at which the card is no longer valid. *)
  val end_ : t -> Time_ns.t

  val is_expired : t -> now:Time_ns.t -> bool

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

type t =
  { last4 : Last4.t
  ; expiry : Expiry.t
  }
[@@deriving sexp, compare, equal, hash, quickcheck]

(** ["**** 4242 10/29"]: what a statement or app may show. *)
val to_string : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
