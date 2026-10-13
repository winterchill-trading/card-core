open! Core
module Utc_time = Payments_core.Utc_time

module Category = struct
  type t =
    | Fraud
    | Authorization
    | Processing_error
    | Consumer
  [@@deriving sexp, compare, equal, hash, enumerate]

  let number = function
    | Fraud -> 10
    | Authorization -> 11
    | Processing_error -> 12
    | Consumer -> 13
  ;;
end

module Reason = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Card_absent_fraud
        | Declined_authorization
        | No_authorization
        | Incorrect_amount
        | Duplicate_processing
        | Not_received
        | Credit_not_processed
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
    end
  end

  include Stable.V1

  let category : t -> Category.t = function
    | Card_absent_fraud -> Fraud
    | Declined_authorization | No_authorization -> Authorization
    | Incorrect_amount | Duplicate_processing -> Processing_error
    | Not_received | Credit_not_processed -> Consumer
  ;;

  let code = function
    | Card_absent_fraud -> "10.4"
    | Declined_authorization -> "11.2"
    | No_authorization -> "11.3"
    | Incorrect_amount -> "12.5"
    | Duplicate_processing -> "12.6"
    | Not_received -> "13.1"
    | Credit_not_processed -> "13.6"
  ;;
end

module Stage = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Filed
        | Response
        | Pre_arbitration
        | Pre_arbitration_response
        | Arbitration
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
    end
  end

  include Stable.V1

  let time_limit = function
    | Arbitration -> Time_ns.Span.of_day 10.
    | Filed | Response | Pre_arbitration | Pre_arbitration_response ->
      Time_ns.Span.of_day 30.
  ;;
end

module Outcome = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Won
        | Lost
        | Withdrawn
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
    end
  end

  include Stable.V1
end

module Status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Open of Stage.Stable.V1.t
        | Closed of Outcome.Stable.V1.t
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Evidence = struct
  module Stable = struct
    module V1 = struct
      type t =
        { kind : string
        ; description : string
        ; at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Ids.Dispute_id.Stable.V1.t
      ; card_id : Ids.Card_id.Stable.V1.t
      ; clearing_id : Ids.Clearing_id.Stable.V1.t
      ; amount : Money.Amount.Stable.V1.t
      ; reason : Reason.Stable.V1.t
      ; status : Status.Stable.V1.t
      ; evidence : Evidence.Stable.V1.t list
      ; opened_at : Utc_time.Stable.V1.t
      ; stage_started_at : Utc_time.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let filing_window = Time_ns.Span.of_day 120.

let open_
  ~id
  ~(clearing : Settlement.Clearing.t)
  ?three_ds
  ~scheme
  ~reason
  ~amount
  ~at
  ()
  =
  let fail msg = Or_error.error_s [%message msg ~dispute:(id : Ids.Dispute_id.t)] in
  match clearing.direction with
  | Credit -> fail "Dispute: only a debit can be disputed"
  | Debit ->
    if Time_ns.( > ) at (Time_ns.add clearing.at filing_window)
    then fail "Dispute: filing window closed"
    else if not (Money.Amount.is_positive amount)
    then fail "Dispute: amount must be positive"
    else if not
              (Money.Currency.equal
                 (Money.Amount.currency amount)
                 (Money.Amount.currency clearing.billing))
            || Money.Amount.( > ) amount clearing.billing
    then fail "Dispute: amount above the cleared amount"
    else if Reason.equal reason Card_absent_fraud
            && Option.exists three_ds ~f:(Three_ds.liability_shift scheme)
    then fail "Dispute: 3-D Secure moved liability to the issuer"
    else
      Ok
        { id
        ; card_id = clearing.card_id
        ; clearing_id = clearing.id
        ; amount
        ; reason
        ; status = Open Filed
        ; evidence = []
        ; opened_at = at
        ; stage_started_at = at
        }
;;

let add_evidence t ~kind ~description ~at =
  let open Or_error.Let_syntax in
  let%bind description = Pan_guard.check ~field:"evidence description" description in
  match t.status with
  | Closed _ -> Or_error.error_s [%message "Dispute: closed" ~dispute:(t.id : Ids.Dispute_id.t)]
  | Open _ -> Ok { t with evidence = t.evidence @ [ { Evidence.kind; description; at } ] }
;;

let allowed ~(category : Category.t) (from : Stage.t) (to_ : Stage.t) =
  match from, to_ with
  | Filed, Response -> (
    match category with
    | Processing_error | Consumer -> true
    | Fraud | Authorization -> false)
  | Filed, Pre_arbitration -> (
    match category with
    | Fraud | Authorization -> true
    | Processing_error | Consumer -> false)
  | Response, Pre_arbitration
  | Pre_arbitration, Pre_arbitration_response
  | Pre_arbitration_response, Arbitration -> true
  | _ -> false
;;

let advance t ~to_ ~at =
  match t.status with
  | Closed _ -> Or_error.error_s [%message "Dispute: closed" ~dispute:(t.id : Ids.Dispute_id.t)]
  | Open from ->
    if allowed ~category:(Reason.category t.reason) from to_
    then Ok { t with status = Open to_; stage_started_at = at }
    else
      Or_error.error_s
        [%message
          "Dispute: stage not allowed"
            ~dispute:(t.id : Ids.Dispute_id.t)
            (from : Stage.t)
            (to_ : Stage.t)
            ~reason:(Reason.code t.reason)]
;;

let close t outcome ~at =
  match t.status with
  | Closed _ -> Or_error.error_s [%message "Dispute: closed" ~dispute:(t.id : Ids.Dispute_id.t)]
  | Open _ -> Ok { t with status = Closed outcome; stage_started_at = at }
;;

let deadline t =
  match t.status with
  | Closed _ -> None
  | Open stage -> Some (Time_ns.add t.stage_started_at (Stage.time_limit stage))
;;
