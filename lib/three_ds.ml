open! Core

module Scheme = struct
  type t =
    | Visa
    | Mastercard
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]
end

module Trans_status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Y
        | A
        | I
        | N
        | U
        | R
        | C
        | D
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1

  let to_string t = Sexp.to_string (sexp_of_t t)

  let of_string_or_error s =
    match List.find all ~f:(fun t -> String.equal (to_string t) s) with
    | Some t -> Ok t
    | None -> Or_error.error_s [%message "Three_ds: unknown transStatus" s]
  ;;
end

module Stable = struct
  module V1 = struct
    type t =
      { trans_status : Trans_status.Stable.V1.t
      ; eci : int option
      ; cavv_present : bool
      }
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]
  end
end

include Stable.V1

let expected_eci (scheme : Scheme.t) (status : Trans_status.t) =
  match scheme, status with
  | Visa, Y -> Some 5
  | Visa, A -> Some 6
  | Visa, (I | N | U) -> Some 7
  | Mastercard, Y -> Some 2
  | Mastercard, A -> Some 1
  (* Mastercard sends 02 for an accepted exemption, as for Y: the issuer took the risk. *)
  | Mastercard, I -> Some 2
  | Mastercard, (N | U) -> Some 0
  | (Visa | Mastercard), (R | C | D) -> None
;;

let liability_shift (scheme : Scheme.t) t =
  t.cavv_present
  &&
  match scheme, t.eci with
  | Visa, Some (5 | 6) -> true
  | Mastercard, Some (1 | 2) -> true
  | (Visa | Mastercard), _ -> false
;;
