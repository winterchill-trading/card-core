open! Core
module Amount = Money.Amount

module Risk = struct
  type verdict =
    | Allow
    | Review of string
    | Block of string
  [@@deriving sexp, compare, equal]
end

module Input = struct
  type t =
    { card : Card.t
    ; usage : Limits.Usage.t
    ; available : Amount.t
    ; sca_policy : Card_sca.Policy.t
    ; sca_counters : Payments_core.Sca.Counters.t
    ; request : Authorization.Request.t
    ; open_auth : Authorization.t option
    }
  [@@deriving sexp_of]
end

type t =
  | Approve of
      { amount : Amount.t
      ; partial : bool
      ; sca : Card_sca.Outcome.t
      }
  | Decline of Decline_reason.t
[@@deriving sexp_of, compare, equal]

(* Each check returns [Error reason] to stop with a decline. *)
let card_check (input : Input.t) ~now : (unit, Decline_reason.t) Result.t =
  let card = input.card in
  if not (Ids.Card_id.equal card.id input.request.card_id)
  then Error Unknown_card
  else (
    match Card.Status.decline_code card.status with
    | Some _ -> Error (Card_status card.status)
    | None ->
      (match Card.unusable card ~now with
       | Some _ -> Error Card_expired
       | None -> Ok ()))
;;

let currency_check (input : Input.t) : (unit, Decline_reason.t) Result.t =
  if Money.Currency.equal (Amount.currency input.request.amount) input.card.currency
     && Money.Currency.equal (Amount.currency input.available) input.card.currency
  then Ok ()
  else Error Wrong_currency
;;

let increment_check (input : Input.t) : (unit, Decline_reason.t) Result.t =
  match input.request.kind with
  | Auth | Advice | Financial -> Ok ()
  | Incremental ->
    (match input.open_auth with
     | Some auth
       when Authorization.Status.is_open auth.status
            && Ids.Network_txn_id.equal auth.network_txn_id input.request.network_txn_id
            && Ids.Card_id.equal auth.card_id input.request.card_id -> Ok ()
     | Some _ | None -> Error Unmatched_increment)
;;

let limits_check (input : Input.t) : (unit, Decline_reason.t) Result.t =
  let r = input.request in
  match
    Limits.check
      input.card.limits
      ~usage:input.usage
      ~amount:r.amount
      ~mcc:r.merchant.mcc
      ~country:r.merchant.country
      ~entry_mode:r.entry_mode
  with
  | Ok (Ok ()) -> Ok ()
  | Ok (Error violation) -> Error (Limit violation)
  (* Currencies were checked just before, so this cannot happen; decline rather than raise
     inside an authorization. *)
  | Error _ -> Error Wrong_currency
;;

(* The amount to approve: all of it, the available balance for a merchant that takes
   partial approvals, or a decline. *)
let funds_check (input : Input.t) : (Amount.t * bool, Decline_reason.t) Result.t =
  let requested = input.request.amount in
  if Amount.( <= ) requested input.available
  then Ok (requested, false)
  else if input.request.partial_supported && Amount.is_positive input.available
  then Ok (input.available, true)
  else Error Insufficient_funds
;;

let sca_check (input : Input.t) : (Card_sca.Outcome.t, Decline_reason.t) Result.t =
  match
    Card_sca.decide
      input.sca_policy
      input.sca_counters
      (Authorization.Request.sca_request input.request)
  with
  | Required -> Error Sca_required
  | Failed -> Error Authentication_failed
  | (Authenticated | Exempt _ | Out_of_scope _) as outcome -> Ok outcome
;;

let decide ?(risk = fun _ -> Risk.Allow) (input : Input.t) ~now =
  let result =
    let open Result.Let_syntax in
    let%bind () = card_check input ~now in
    let%bind () = currency_check input in
    let%bind () = increment_check input in
    let%bind () = limits_check input in
    let%bind amount, partial = funds_check input in
    let%bind sca = sca_check input in
    match risk input with
    | Allow -> Ok (Approve { amount; partial; sca })
    | Review rule -> Error (Decline_reason.Risk_review rule)
    | Block rule -> Error (Risk_block rule)
  in
  match result with
  | Ok approve -> approve
  | Error reason -> Decline reason
;;

let response_code = function
  | Approve _ -> Response_code.Approved
  | Decline reason -> Decline_reason.response_code reason
;;

let apply t auth ~(request : Authorization.Request.t) =
  match t with
  | Approve { amount; partial = _; sca } ->
    Authorization.approve
      auth
      ~amount
      ~sca:(Card_sca.Outcome.evidence sca (Authorization.Request.sca_request request))
      ~decided_by:Us
      ~at:request.at
  | Decline reason -> Authorization.decline auth reason ~decided_by:Us ~at:request.at
;;
