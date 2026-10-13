open! Core
module Amount = Money.Amount

module Direction = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Debit
        | Credit
      [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
    end
  end

  include Stable.V1
end

module Fx = struct
  module Stable = struct
    module V1 = struct
      type t =
        { rate : Money.Rate.Stable.V1.t
        ; fee : Amount.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Clearing = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : Ids.Clearing_id.Stable.V1.t
        ; card_id : Ids.Card_id.Stable.V1.t
        ; network_txn_id : Ids.Network_txn_id.Stable.V1.t option
        ; auth_id : Ids.Auth_id.Stable.V1.t option
        ; direction : Direction.Stable.V1.t
        ; merchant : Merchant.Stable.V1.t
        ; billing : Amount.Stable.V1.t
        ; fx : Fx.Stable.V1.t option
        ; final : bool
        ; at : Payments_core.Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let create
    ~id
    ~card_id
    ?network_txn_id
    ?auth_id
    ~direction
    ~(merchant : Merchant.t)
    ~billing_currency
    ?billing
    ?rate
    ?fx_fee
    ?(final = true)
    ~at
    ()
    =
    let open Or_error.Let_syntax in
    let merchant_currency = Amount.currency merchant.amount in
    let%bind () =
      if Amount.is_positive merchant.amount
      then Ok ()
      else
        Or_error.error_s
          [%message
            "Clearing: amount must be positive" ~clearing:(id : Ids.Clearing_id.t)]
    in
    let%bind billing, fx =
      if Money.Currency.equal merchant_currency billing_currency
      then Ok (Option.value billing ~default:merchant.amount, None)
      else (
        match rate with
        | None ->
          Or_error.error_s
            [%message
              "Clearing: a conversion needs the rate"
                ~merchant:(merchant_currency : Money.Currency.t)
                ~billing:(billing_currency : Money.Currency.t)]
        | Some rate ->
          let%bind () =
            if Money.Currency.equal (Money.Rate.base rate) merchant_currency
               && Money.Currency.equal (Money.Rate.quote rate) billing_currency
            then Ok ()
            else
              Or_error.error_s
                [%message
                  "Clearing: rate for another currency pair" (rate : Money.Rate.t)]
          in
          let%bind billing =
            match billing with
            | Some billing -> Ok billing
            | None -> Money.Rate.convert rate merchant.amount
          in
          let%map fee =
            match fx_fee, direction with
            | Some schedule, Direction.Debit ->
              Money.Fee.compute schedule ~side:Receivable billing
            | None, _ | Some _, Direction.Credit -> Ok (Amount.zero billing_currency)
          in
          billing, Some { Fx.rate; fee })
    in
    let%bind () =
      if Money.Currency.equal (Amount.currency billing) billing_currency
         && Amount.is_positive billing
      then Ok ()
      else Or_error.error_s [%message "Clearing: bad billing amount" (billing : Amount.t)]
    in
    let%bind () =
      match fx with
      | Some { fee; _ }
        when not (Money.Currency.equal (Amount.currency fee) billing_currency) ->
        Or_error.error_s
          [%message "Clearing: FX fee in another currency" (fee : Amount.t)]
      | _ -> Ok ()
    in
    return
      { id
      ; card_id
      ; network_txn_id
      ; auth_id
      ; direction
      ; merchant
      ; billing
      ; fx
      ; final
      ; at
      }
  ;;

  let total_charged t =
    match t.fx with
    | None -> t.billing
    | Some { fee; _ } -> Amount.add_exn t.billing fee
  ;;
end

let excess_review_bps = 2000

module Outcome = struct
  type t =
    | Against_auth of
        { auth : Authorization.t
        ; applied : Authorization.Applied.t
        ; review_excess : bool
        }
    | Without_auth
    | Refund of { auth : Authorization.t option }
  [@@deriving sexp_of]
end

let usable_auth (clearing : Clearing.t) (auth : Authorization.t option) =
  Option.filter auth ~f:(fun (auth : Authorization.t) ->
    Ids.Card_id.equal auth.card_id clearing.card_id
    &&
    match auth.status with
    | Pending | Declined _ -> false
    | Authorized | Partially_cleared | Cleared | Reversed | Expired -> true)
;;

let settle (clearing : Clearing.t) ~auth : Outcome.t Or_error.t =
  let open Or_error.Let_syntax in
  let at = clearing.at in
  match clearing.direction, usable_auth clearing auth with
  | Credit, None -> Ok (Refund { auth = None })
  | Credit, Some auth ->
    let%map auth = Authorization.record_refund auth ~amount:clearing.billing ~at in
    Outcome.Refund { auth = Some auth }
  | Debit, None -> Ok Without_auth
  | Debit, Some auth ->
    (* A hold that ran out before the clearing arrived is released first, as the ledger
       does; the clearing then takes nothing from it. *)
    let%bind auth =
      if Authorization.is_due_to_expire auth ~now:at
      then Authorization.expire auth ~now:at |> Or_error.map ~f:fst
      else Ok auth
    in
    let%map updated, applied =
      Authorization.apply_clearing auth ~amount:clearing.billing ~final:clearing.final ~at
    in
    let threshold = Amount.mul_bps auth.approved ~bps:excess_review_bps ~rounding:Down in
    Outcome.Against_auth
      { auth = updated; applied; review_excess = Amount.( > ) applied.excess threshold }
;;
