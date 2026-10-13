open! Core
module Amount = Money.Amount
module Utc_time = Payments_core.Utc_time

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Auth
        | Incremental
        | Advice
        | Financial
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1
end

module Decided_by = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Us
        | Stand_in
        | Provider_default
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
    end
  end

  include Stable.V1
end

module Status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Pending
        | Authorized
        | Partially_cleared
        | Cleared
        | Declined of Decline_reason.Stable.V1.t
        | Reversed
        | Expired
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let is_open = function
    | Authorized | Partially_cleared -> true
    | Pending | Cleared | Declined _ | Reversed | Expired -> false
  ;;
end

module Request = struct
  type t =
    { auth_id : Ids.Auth_id.t
    ; card_id : Ids.Card_id.t
    ; network_txn_id : Ids.Network_txn_id.t
    ; kind : Kind.t
    ; amount : Amount.t
    ; merchant : Merchant.t
    ; entry_mode : Entry_mode.t
    ; initiator : Card_sca.Initiator.t
    ; estimated : bool
    ; partial_supported : bool
    ; three_ds : Three_ds.t option
    ; exemption : Payments_core.Sca.Exemption.t option
    ; cardholder_verified : bool
    ; at : Utc_time.t
    }
  [@@deriving sexp_of]

  let sca_request (t : t) : Card_sca.Request.t =
    { entry_mode = t.entry_mode
    ; initiator = t.initiator
    ; amount = t.amount
    ; three_ds = t.three_ds
    ; exemption = t.exemption
    ; cardholder_verified = t.cardholder_verified
    }
  ;;
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Ids.Auth_id.Stable.V1.t
      ; card_id : Ids.Card_id.Stable.V1.t
      ; network_txn_id : Ids.Network_txn_id.Stable.V1.t
      ; kind : Kind.Stable.V1.t
      ; requested : Amount.Stable.V1.t
      ; approved : Amount.Stable.V1.t
      ; merchant : Merchant.Stable.V1.t
      ; entry_mode : Entry_mode.Stable.V1.t
      ; initiator : Card_sca.Initiator.Stable.V1.t
      ; estimated : bool
      ; three_ds : Three_ds.Stable.V1.t option
      ; sca : Ledger.Sca_evidence.Stable.V1.t option
      ; response_code : Response_code.Stable.V1.t option
      ; decided_by : Decided_by.Stable.V1.t
      ; status : Status.Stable.V1.t
      ; increments : Ids.Auth_id.Stable.V1.t list
      ; captured : Amount.Stable.V1.t
      ; cleared : Amount.Stable.V1.t
      ; released : Amount.Stable.V1.t
      ; refunded : Amount.Stable.V1.t
      ; created_at : Utc_time.Stable.V1.t
      ; expires_at : Utc_time.Stable.V1.t
      ; updated_at : Utc_time.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let hold_validity ~mcc ~estimated =
  if estimated || Mcc.has_long_hold mcc then Time_ns.Span.of_day 30. else Time_ns.Span.of_day 7.
;;

let currency t = Amount.currency t.requested
let zero t = Amount.zero (currency t)

let open_hold t =
  if Status.is_open t.status
  then Amount.sub_exn (Amount.sub_exn t.approved t.captured) t.released
  else zero t
;;

let spend t = Amount.add_exn t.cleared (open_hold t)

let positive what amount =
  if Amount.is_positive amount
  then Ok ()
  else Or_error.error_s [%message "Authorization: amount must be positive" what (amount : Amount.t)]
;;

let same_currency t amount =
  if Money.Currency.equal (Amount.currency amount) (currency t)
  then Ok ()
  else
    Or_error.error_s
      [%message
        "Authorization: amount in another currency"
          ~auth:(t.id : Ids.Auth_id.t)
          (amount : Amount.t)]
;;

let of_request (r : Request.t) ~approved ~sca ~response_code ~decided_by ~status =
  let zero = Amount.zero (Amount.currency r.amount) in
  { id = r.auth_id
  ; card_id = r.card_id
  ; network_txn_id = r.network_txn_id
  ; kind = r.kind
  ; requested = r.amount
  ; approved
  ; merchant = r.merchant
  ; entry_mode = r.entry_mode
  ; initiator = r.initiator
  ; estimated = r.estimated
  ; three_ds = r.three_ds
  ; sca
  ; response_code
  ; decided_by
  ; status
  ; increments = []
  ; captured = zero
  ; cleared = zero
  ; released = zero
  ; refunded = zero
  ; created_at = r.at
  ; expires_at =
      Time_ns.add r.at (hold_validity ~mcc:r.merchant.mcc ~estimated:r.estimated)
  ; updated_at = r.at
  }
;;

let pending (r : Request.t) =
  let open Or_error.Let_syntax in
  let%bind () = positive "requested" r.amount in
  match r.kind with
  | Incremental ->
    Or_error.error_s
      [%message
        "Authorization: an incremental request adds to an open authorization"
          ~auth:(r.auth_id : Ids.Auth_id.t)]
  | Auth | Advice | Financial ->
    Ok
      (of_request
         r
         ~approved:(Amount.zero (Amount.currency r.amount))
         ~sca:None
         ~response_code:None
         ~decided_by:Us
         ~status:Pending)
;;

let expect_status t ~expected =
  if List.mem expected t.status ~equal:Status.equal
  then Ok ()
  else
    Or_error.error_s
      [%message
        "Authorization: not allowed in this state"
          ~auth:(t.id : Ids.Auth_id.t)
          ~status:(t.status : Status.t)]
;;

let approve t ~amount ~sca ~decided_by ~at =
  let open Or_error.Let_syntax in
  let%bind () = expect_status t ~expected:[ Pending ] in
  let%bind () = same_currency t amount in
  let%bind () = positive "approved" amount in
  if Amount.( > ) amount t.requested
  then
    Or_error.error_s
      [%message
        "Authorization: approved above the requested amount"
          (amount : Amount.t)
          ~requested:(t.requested : Amount.t)]
  else
    Ok
      { t with
        approved = amount
      ; sca
      ; decided_by
      ; response_code = Some Approved
      ; status = Authorized
      ; updated_at = at
      }
;;

let decline t reason ~decided_by ~at =
  let open Or_error.Let_syntax in
  let%map () = expect_status t ~expected:[ Pending ] in
  { t with
    status = Declined reason
  ; response_code = Some (Decline_reason.response_code reason)
  ; decided_by
  ; updated_at = at
  }
;;

let advice (r : Request.t) ~approved ~response_code ~decided_by =
  let open Or_error.Let_syntax in
  let%bind () = positive "requested" r.amount in
  let zero = Amount.zero (Amount.currency r.amount) in
  if Response_code.is_approved response_code
  then (
    let%bind () = positive "approved" approved in
    if not (Money.Currency.equal (Amount.currency approved) (Amount.currency r.amount))
    then Or_error.error_s [%message "Authorization: advice in another currency" (approved : Amount.t)]
    else
      Ok
        (of_request
           r
           ~approved
           ~sca:None
           ~response_code:(Some response_code)
           ~decided_by
           ~status:Authorized))
  else
    Ok
      (of_request
         r
         ~approved:zero
         ~sca:None
         ~response_code:(Some response_code)
         ~decided_by
         ~status:(Declined (Declined_upstream response_code)))
;;

let increment t (r : Request.t) ~approved =
  let open Or_error.Let_syntax in
  let%bind () = expect_status t ~expected:[ Authorized; Partially_cleared ] in
  let%bind () = same_currency t r.amount in
  let%bind () = same_currency t approved in
  let%bind () = positive "approved" approved in
  if not (Ids.Card_id.equal t.card_id r.card_id && Ids.Network_txn_id.equal t.network_txn_id r.network_txn_id)
  then
    Or_error.error_s
      [%message
        "Authorization: increment for another card or transaction"
          ~auth:(t.id : Ids.Auth_id.t)
          ~increment:(r.auth_id : Ids.Auth_id.t)]
  else if Amount.( > ) approved r.amount
  then Or_error.error_s [%message "Authorization: approved above the requested increment"]
  else if List.mem t.increments r.auth_id ~equal:Ids.Auth_id.equal
  then Or_error.error_s [%message "Authorization: increment already applied" ~increment:(r.auth_id : Ids.Auth_id.t)]
  else
    Ok
      { t with
        requested = Amount.add_exn t.requested r.amount
      ; approved = Amount.add_exn t.approved approved
      ; increments = t.increments @ [ r.auth_id ]
      ; updated_at = r.at
      }
;;

(* After a release or a clearing: an authorization with nothing left on hold is closed,
   as [Cleared] when anything cleared, else by what released it. *)
let settle_status t ~closed_as =
  if Amount.is_zero (Amount.sub_exn (Amount.sub_exn t.approved t.captured) t.released)
  then if Amount.is_positive t.cleared then Status.Cleared else closed_as
  else if Amount.is_positive t.cleared
  then Partially_cleared
  else t.status
;;

let reverse t ?amount ~at () =
  let open Or_error.Let_syntax in
  let%bind () = expect_status t ~expected:[ Authorized; Partially_cleared ] in
  let hold = open_hold t in
  let amount = Option.value amount ~default:hold in
  let%bind () = same_currency t amount in
  let%bind () = positive "reversal" amount in
  if Amount.( > ) amount hold
  then
    Or_error.error_s
      [%message
        "Authorization: reversal above the open hold" (amount : Amount.t) ~open_hold:(hold : Amount.t)]
  else (
    let t = { t with released = Amount.add_exn t.released amount; updated_at = at } in
    Ok ({ t with status = settle_status t ~closed_as:Reversed }, amount))
;;

let is_due_to_expire t ~now =
  Status.is_open t.status
  && Time_ns.( >= ) now t.expires_at
  && Amount.is_positive (open_hold t)
;;

let expire t ~now =
  if not (is_due_to_expire t ~now)
  then
    Or_error.error_s
      [%message
        "Authorization: not due to expire"
          ~auth:(t.id : Ids.Auth_id.t)
          ~expires_at:(t.expires_at : Utc_time.t)
          ~status:(t.status : Status.t)]
  else (
    let hold = open_hold t in
    let t = { t with released = Amount.add_exn t.released hold; updated_at = now } in
    Ok ({ t with status = settle_status t ~closed_as:Expired }, hold))
;;

module Applied = struct
  type t =
    { from_hold : Amount.t
    ; excess : Amount.t
    ; released : Amount.t
    }
  [@@deriving sexp_of, compare, equal]
end

let apply_clearing t ~amount ~final ~at =
  let open Or_error.Let_syntax in
  let%bind () =
    expect_status t ~expected:[ Authorized; Partially_cleared; Cleared; Reversed; Expired ]
  in
  let%bind () = same_currency t amount in
  let%bind () = positive "clearing" amount in
  let hold = open_hold t in
  let from_hold = Amount.min amount hold in
  let excess = Amount.sub_exn amount from_hold in
  let released = if final then Amount.sub_exn hold from_hold else zero t in
  let t =
    { t with
      captured = Amount.add_exn t.captured from_hold
    ; cleared = Amount.add_exn t.cleared amount
    ; released = Amount.add_exn t.released released
    ; updated_at = at
    }
  in
  let status =
    match t.status with
    (* A late clearing after the hold was released: the authorization is now cleared. *)
    | Reversed | Expired | Cleared -> Status.Cleared
    | Pending | Authorized | Partially_cleared | Declined _ ->
      settle_status t ~closed_as:Cleared
  in
  return ({ t with status }, { Applied.from_hold; excess; released })
;;

let record_refund t ~amount ~at =
  let open Or_error.Let_syntax in
  let%bind () = same_currency t amount in
  let%bind () = positive "refund" amount in
  match t.status with
  | Pending | Declined _ ->
    Or_error.error_s
      [%message "Authorization: refund for an authorization that never cleared" ~auth:(t.id : Ids.Auth_id.t)]
  | Authorized | Partially_cleared | Cleared | Reversed | Expired ->
    Ok { t with refunded = Amount.add_exn t.refunded amount; updated_at = at }
;;

let usage ts ~currency ~now =
  let day = Time_ns.to_date now ~zone:Time_float.Zone.utc in
  let open Or_error.Let_syntax in
  List.fold_result ts ~init:(Limits.Usage.zero currency) ~f:(fun (usage : Limits.Usage.t) t ->
    let created = Time_ns.to_date t.created_at ~zone:Time_float.Zone.utc in
    let same_month =
      Date.year created = Date.year day && Month.equal (Date.month created) (Date.month day)
    in
    let spend = spend t in
    if (not same_month) || Amount.is_zero spend
    then Ok usage
    else (
      let%bind () =
        if Money.Currency.equal (Amount.currency spend) currency
        then Ok ()
        else Or_error.error_s [%message "Authorization.usage: another currency" ~auth:(t.id : Ids.Auth_id.t)]
      in
      let%map monthly = Amount.add usage.monthly spend in
      if Date.equal created day
      then (
        let atm = Mcc.is_atm t.merchant.mcc in
        { Limits.Usage.daily = Amount.add_exn usage.daily spend
        ; monthly
        ; atm_daily = (if atm then Amount.add_exn usage.atm_daily spend else usage.atm_daily)
        ; atm_daily_count = (usage.atm_daily_count + if atm then 1 else 0)
        })
      else { usage with monthly }))
;;
