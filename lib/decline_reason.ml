open! Core

module Stable = struct
  module V1 = struct
    module Violation = struct
      type t = Limits.Violation.t =
        | Per_transaction
        | Daily
        | Monthly
        | Atm_amount
        | Atm_count
        | Atm_disabled
        | Online_disabled
        | Contactless_disabled
        | Mcc_blocked of Mcc.Stable.V1.t
        | Country_blocked of Country.Stable.V1.t
      [@@deriving bin_io, sexp, compare, equal]
    end

    type t =
      | Unknown_card
      | Card_status of Card.Status.Stable.V1.t
      | Card_expired
      | Wrong_currency
      | Limit of Violation.t
      | Insufficient_funds
      | Sca_required
      | Authentication_failed
      | Risk_review of string
      | Risk_block of string
      | Unmatched_increment
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let response_code : t -> Response_code.t = function
  | Unknown_card -> Invalid_account
  | Card_status status ->
    Card.Status.decline_code status |> Option.value ~default:Response_code.Do_not_honor
  | Card_expired -> Expired_card
  | Limit violation -> Limits.Violation.response_code violation
  | Insufficient_funds -> Insufficient_funds
  | Sca_required -> Sca_required
  | Wrong_currency
  | Authentication_failed
  | Risk_review _
  | Risk_block _
  | Unmatched_increment -> Do_not_honor
;;
