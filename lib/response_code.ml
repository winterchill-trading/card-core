open! Core

module T = struct
  type t =
    | Approved
    | Do_not_honor
    | Invalid_account
    | Closed_account
    | Insufficient_funds
    | Expired_card
    | Incorrect_pin
    | Not_permitted
    | Exceeds_amount_limit
    | Restricted_card
    | Exceeds_frequency_limit
    | Fraud_security
    | Issuer_unavailable
    | Sca_required
    | Other of string
  [@@deriving compare, equal, hash]
end

include T

let all_known =
  [ Approved
  ; Do_not_honor
  ; Invalid_account
  ; Closed_account
  ; Insufficient_funds
  ; Expired_card
  ; Incorrect_pin
  ; Not_permitted
  ; Exceeds_amount_limit
  ; Restricted_card
  ; Exceeds_frequency_limit
  ; Fraud_security
  ; Issuer_unavailable
  ; Sca_required
  ]
;;

let to_string = function
  | Approved -> "00"
  | Do_not_honor -> "05"
  | Invalid_account -> "14"
  | Closed_account -> "46"
  | Insufficient_funds -> "51"
  | Expired_card -> "54"
  | Incorrect_pin -> "55"
  | Not_permitted -> "57"
  | Exceeds_amount_limit -> "61"
  | Restricted_card -> "62"
  | Exceeds_frequency_limit -> "65"
  | Fraud_security -> "83"
  | Issuer_unavailable -> "91"
  | Sca_required -> "1A"
  | Other s -> s
;;

let known_by_text =
  List.map all_known ~f:(fun t -> to_string t, t) |> String.Map.of_alist_exn
;;

let of_string_or_error s =
  let valid_char c = Char.is_digit c || Char.is_uppercase c in
  if String.length s <> 2 || not (String.for_all s ~f:valid_char)
  then Or_error.error_s [%message "Response_code: expected two characters [0-9A-Z]" s]
  else Ok (Map.find known_by_text s |> Option.value ~default:(Other s))
;;

let is_approved = equal Approved

module Category = struct
  type t =
    | Never_approve
    | Cannot_approve_now
    | Data_quality
    | Generic
  [@@deriving sexp, compare, equal, enumerate]
end

let category : t -> Category.t option = function
  | Approved -> None
  | Invalid_account | Closed_account | Not_permitted | Restricted_card | Fraud_security ->
    Some Never_approve
  | Insufficient_funds
  | Exceeds_amount_limit
  | Exceeds_frequency_limit
  | Issuer_unavailable -> Some Cannot_approve_now
  | Expired_card | Incorrect_pin | Sca_required -> Some Data_quality
  | Do_not_honor | Other _ -> Some Generic
;;

let retry_allowed t =
  match category t with
  | Some Never_approve -> false
  | None | Some (Cannot_approve_now | Data_quality | Generic) -> true
;;

let sexp_of_t t = Sexp.Atom (to_string t)

let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom s -> of_string_or_error s |> Or_error.ok_exn
  | Sexp.List _ -> of_sexp_error "Response_code: expected an atom" sexp
;;

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, hash]

    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (String)
        (struct
          type nonrec t = t

          let to_binable = to_string
          let of_binable s = of_string_or_error s |> Or_error.ok_exn
        end)
  end
end
