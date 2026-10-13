open! Core

module T = struct
  type t = string [@@deriving compare, equal, hash]

  let to_string t = t

  let of_string_or_error s =
    if String.length s = 2 && String.for_all s ~f:Char.is_uppercase
    then Ok s
    else Or_error.error_s [%message "Country: expected two upper-case letters" s]
  ;;

  let of_string s = of_string_or_error s |> Or_error.ok_exn
  let sexp_of_t t = Sexp.Atom t

  let t_of_sexp = function
    | Sexp.Atom s -> of_string s
    | Sexp.List _ as sexp -> of_sexp_error "Country: expected an atom" sexp
  ;;
end

include T
include Comparable.Make (T)

let quickcheck_generator =
  Quickcheck.Generator.of_list [ "DE"; "FR"; "NL"; "ES"; "IT"; "GB"; "US"; "LT" ]
;;

let quickcheck_observer = String.quickcheck_observer
let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, hash]

    let sexp_of_t = T.sexp_of_t
    let t_of_sexp = T.t_of_sexp

    (* Same bytes as a plain string, but decoding checks the value again. *)
    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (String)
        (struct
          type nonrec t = t

          let to_binable t = t
          let of_binable x = T.of_string_or_error x |> Or_error.ok_exn
        end)
  end
end
