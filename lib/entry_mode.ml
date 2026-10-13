open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Chip
      | Contactless
      | Magstripe
      | Manual
      | Ecommerce
      | Token
    [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
  end
end

include Stable.V1

(* A token can also be tapped at a terminal (a phone wallet); the processor then reports
   the entry mode as contactless, so [Token] here means a remote card-on-file use. *)
let is_card_absent = function
  | Manual | Ecommerce | Token -> true
  | Chip | Contactless | Magstripe -> false
;;
