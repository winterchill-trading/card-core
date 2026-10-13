open! Core

module Kind = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Virtual
        | Physical
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1
end

module Status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Requested
        | Active
        | Frozen
        | Blocked
        | Expired
        | Destroyed
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
    end
  end

  include Stable.V1

  let transitions =
    [ Requested, [ Active; Destroyed ]
    ; Active, [ Frozen; Blocked; Expired; Destroyed ]
    ; Frozen, [ Active; Blocked; Expired; Destroyed ]
    ; Blocked, [ Destroyed ]
    ; Expired, [ Destroyed ]
    ; Destroyed, []
    ]
  ;;

  let can_transition ~from ~to_ =
    List.mem (List.Assoc.find_exn transitions from ~equal) to_ ~equal
  ;;

  let is_terminal t = List.is_empty (List.Assoc.find_exn transitions t ~equal)

  let decline_code : t -> Response_code.t option = function
    | Active -> None
    | Requested | Frozen -> Some Not_permitted
    | Blocked -> Some Restricted_card
    | Expired -> Some Expired_card
    | Destroyed -> Some Closed_account
  ;;
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Ids.Card_id.Stable.V1.t
      ; holder : string
      ; account : Ledger.Account_id.Stable.V1.t
      ; currency : Money.Currency.Stable.V1.t
      ; kind : Kind.Stable.V1.t
      ; status : Status.Stable.V1.t
      ; masked_pan : Masked_pan.Stable.V1.t option
      ; limits : Limits.Stable.V1.t
      ; created_at : Payments_core.Utc_time.Stable.V1.t
      ; updated_at : Payments_core.Utc_time.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let request ~id ~holder ~account ~currency ~kind ?limits ~at () =
  let limits = Option.value limits ~default:(Limits.unlimited currency) in
  if not (Money.Currency.equal limits.currency currency)
  then
    Or_error.error_s
      [%message
        "Card: limits in another currency"
          ~limits:(limits.currency : Money.Currency.t)
          (currency : Money.Currency.t)]
  else if String.is_empty holder
  then Or_error.error_string "Card: empty holder id"
  else
    Ok
      { id
      ; holder
      ; account
      ; currency
      ; kind
      ; status = Requested
      ; masked_pan = None
      ; limits
      ; created_at = at
      ; updated_at = at
      }
;;

let transition t ~to_ ~at =
  if Status.equal t.status to_
  then Ok t
  else if Status.can_transition ~from:t.status ~to_
  then Ok { t with status = to_; updated_at = at }
  else
    Or_error.error_s
      [%message
        "Card: transition not allowed"
          ~card:(t.id : Ids.Card_id.t)
          ~from:(t.status : Status.t)
          (to_ : Status.t)]
;;

let activate t ~masked_pan ~at =
  let open Or_error.Let_syntax in
  let%map t = transition t ~to_:Active ~at in
  { t with masked_pan = Some masked_pan }
;;

let freeze t ~at = transition t ~to_:Frozen ~at
let unfreeze t ~at = transition t ~to_:Active ~at
let block t ~at = transition t ~to_:Blocked ~at
let destroy t ~at = transition t ~to_:Destroyed ~at

let set_limits t (limits : Limits.t) ~at =
  if Status.is_terminal t.status
  then Or_error.error_s [%message "Card: closed card" ~card:(t.id : Ids.Card_id.t)]
  else if not (Money.Currency.equal limits.currency t.currency)
  then
    Or_error.error_s
      [%message "Card: limits in another currency" ~card:(t.id : Ids.Card_id.t)]
  else Ok { t with limits; updated_at = at }
;;

let past_expiry t ~now =
  match t.masked_pan with
  | None -> false
  | Some { expiry; _ } -> Masked_pan.Expiry.is_expired expiry ~now
;;

let unusable t ~now =
  match Status.decline_code t.status with
  | Some code -> Some code
  | None -> if past_expiry t ~now then Some Response_code.Expired_card else None
;;

let expire_if_due t ~now =
  match t.status with
  | (Active | Frozen) when past_expiry t ~now ->
    { t with status = Expired; updated_at = now }
  | Requested | Active | Frozen | Blocked | Expired | Destroyed -> t
;;
