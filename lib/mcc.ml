open! Core

module T = struct
  type t = int [@@deriving compare, equal, hash]

  let to_string t = sprintf "%04d" t

  let of_int i =
    if i >= 0 && i <= 9999
    then Ok i
    else Or_error.error_s [%message "Mcc: expected 0000 to 9999" (i : int)]
  ;;

  let of_string_or_error s =
    if String.length s = 4 && String.for_all s ~f:Char.is_digit
    then of_int (Int.of_string s)
    else Or_error.error_s [%message "Mcc: expected four digits" s]
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)

  let t_of_sexp = function
    | Sexp.Atom s -> of_string_or_error s |> Or_error.ok_exn
    | Sexp.List _ as sexp -> of_sexp_error "Mcc: expected an atom" sexp
  ;;
end

include T
include Comparable.Make (T)

let of_int_exn i = of_int i |> Or_error.ok_exn
let to_int t = t
let quickcheck_generator = Int.gen_incl 0 9999
let quickcheck_observer = Int.quickcheck_observer
let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
let atm = 6011
let quasi_cash = 6051
let money_transfer = 4829
let stored_value = 6540
let gambling = 7995
let lodging = 7011
let vehicle_rental = 7512
let cruise = 4411
let fuel = 5542
let is_atm t = t = atm
let has_long_hold t = t = lodging || t = vehicle_rental || t = cruise

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, hash]

    let sexp_of_t = T.sexp_of_t
    let t_of_sexp = T.t_of_sexp

    (* Same bytes as a plain int, but decoding checks the value again. *)
    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Int)
        (struct
          type nonrec t = t

          let to_binable t = t
          let of_binable x = T.of_int x |> Or_error.ok_exn
        end)
  end
end
