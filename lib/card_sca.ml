open! Core
module Exemption = Payments_core.Sca.Exemption
module Counters = Payments_core.Sca.Counters

module Initiator = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Cardholder
        | Merchant
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1
end

module Policy = struct
  type t =
    { tra : bool
    ; trusted_beneficiary : bool
    ; corporate : bool
    }
  [@@deriving sexp, compare, equal]

  let default = { tra = false; trusted_beneficiary = false; corporate = false }
end

module Request = struct
  type t =
    { entry_mode : Entry_mode.t
    ; initiator : Initiator.t
    ; amount : Money.Amount.t
    ; three_ds : Three_ds.t option
    ; exemption : Exemption.t option
    ; cardholder_verified : bool
    }
  [@@deriving sexp_of]
end

module Out_of_scope = struct
  type t =
    | Merchant_initiated
    | Mail_or_telephone_order
  [@@deriving sexp, compare, equal, hash]
end

module Outcome = struct
  type t =
    | Authenticated
    | Exempt of Exemption.t
    | Out_of_scope of Out_of_scope.t
    | Required
    | Failed
  [@@deriving sexp_of, compare, equal]

  let evidence t (request : Request.t) : Ledger.Sca_evidence.t option =
    match t with
    | Authenticated ->
      Some
        (Challenge
           (if Option.is_some request.three_ds then "3ds" else "cardholder_verified"))
    | Exempt exemption ->
      Some
        (Exemption
           (sprintf
              "%s %s"
              (Exemption.article exemption)
              (Sexp.to_string [%sexp (exemption : Exemption.t)])))
    | Out_of_scope reason ->
      Some (Exemption (Sexp.to_string [%sexp "out_of_scope", (reason : Out_of_scope.t)]))
    | Required | Failed -> None
  ;;
end

let eur_amount (amount : Money.Amount.t) =
  Money.Currency.equal (Money.Amount.currency amount) Money.Currency.eur
;;

(* TRA bands are euro amounts (RTS art. 18 and annex). *)
let etv_limit : Exemption.Etv.t -> Money.Amount.t = function
  | Eur_100 -> Money.Amount.of_minor_units_int Money.Currency.eur 100_00
  | Eur_250 -> Money.Amount.of_minor_units_int Money.Currency.eur 250_00
  | Eur_500 -> Money.Amount.of_minor_units_int Money.Currency.eur 500_00
;;

let remote_exemption (policy : Policy.t) counters (request : Request.t) : Outcome.t =
  match request.exemption with
  | None -> Required
  | Some exemption ->
    let ok =
      match exemption with
      | Low_value_remote -> Counters.low_value_remote_ok counters ~amount:request.amount
      | Recurring_same_amount_payee -> true
      | Trusted_beneficiary -> policy.trusted_beneficiary
      | Corporate_process -> policy.corporate
      | Tra { band; score = _ } ->
        policy.tra
        && eur_amount request.amount
        && Money.Amount.( <= ) request.amount (etv_limit band)
      | Contactless
      | Unattended_transport_parking
      | Account_info_direct
      | Account_info_aisp
      | Own_accounts -> false
    in
    if ok then Exempt exemption else Required
;;

let present_exemption counters (request : Request.t) : Outcome.t =
  match request.entry_mode, request.exemption with
  | Contactless, _ ->
    if Counters.contactless_ok counters ~amount:request.amount
    then Exempt Contactless
    else Required
  | (Chip | Magstripe), Some Unattended_transport_parking ->
    Exempt Unattended_transport_parking
  | (Chip | Magstripe | Manual | Ecommerce | Token), _ -> Required
;;

let decide policy counters (request : Request.t) : Outcome.t =
  match request.initiator, request.entry_mode with
  | Merchant, _ -> Out_of_scope Merchant_initiated
  | Cardholder, Manual -> Out_of_scope Mail_or_telephone_order
  | Cardholder, _ ->
    (match request.three_ds with
     | Some { trans_status = Y; _ } -> Authenticated
     | Some { trans_status = N | R; _ } -> Failed
     | Some { trans_status = A | I | U | C | D; _ } | None ->
       if request.cardholder_verified
       then Authenticated
       else if Entry_mode.is_card_absent request.entry_mode
       then remote_exemption policy counters request
       else present_exemption counters request)
;;

let counters_after counters (request : Request.t) (outcome : Outcome.t) =
  match outcome with
  | Authenticated -> Counters.reset
  | Exempt Contactless -> Counters.record_contactless counters ~amount:request.amount
  | Exempt Low_value_remote -> Counters.record_remote counters ~amount:request.amount
  | Exempt _ | Out_of_scope _ | Required | Failed -> counters
;;
