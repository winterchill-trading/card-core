(** How the card details reached the terminal or the merchant (ISO 8583 POS entry mode,
    simplified). Card controls (online and contactless toggles) and SCA rules depend on
    it. *)

open! Core

type t =
  | Chip
  | Contactless
  | Magstripe
  | Manual (** keyed in: mail or telephone order *)
  | Ecommerce
  | Token (** a network or device token (mobile wallet, card on file) *)
[@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

(** The card is not at a terminal: e-commerce, keyed-in orders and tokens used remotely.
    Blocked by the online toggle. *)
val is_card_absent : t -> bool

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
