open! Core
module Amount = Money.Amount
module Account_id = Ledger.Account_id

module Chart = struct
  type t =
    { card_settlement : Money.Currency.t -> Account_id.t
    ; card_receivable : Money.Currency.t -> Account_id.t
    ; fee_income : Money.Currency.t -> Account_id.t
    }

  let id prefix currency =
    (* Currency text forms ("EUR", "USDC@polygon") only use ledger id characters. *)
    Account_id.of_string (prefix ^ Money.Currency.to_string currency)
  ;;

  let default =
    { card_settlement = id "clearing:card/"
    ; card_receivable = id "receivable:card/"
    ; fee_income = id "income:fees/"
    }
  ;;

  let accounts t ~currencies =
    List.concat_map currencies ~f:(fun currency ->
      [ Ledger.Account.create
          ~purpose:Clearing
          ~allow_negative:true
          (t.card_settlement currency)
          Asset
      ; Ledger.Account.create (t.card_receivable currency) Asset
      ; Ledger.Account.create ~purpose:Fee_income (t.fee_income currency) Income
      ])
    |> List.dedup_and_sort ~compare:(fun (a : Ledger.Account.t) b ->
      Account_id.compare a.id b.id)
  ;;
end

let hold_id auth_id =
  Ledger.Hold_id.of_string_or_error ("card-auth:" ^ Ids.Auth_id.to_string auth_id)
;;

let key ~event_id leg =
  Ledger.Idempotency_key.of_string_or_error (sprintf "%s/%s" event_id leg)
;;

let reference_of_auth (auth : Authorization.t) = Ids.Auth_id.to_string auth.id

(* Builds legs one after the other against a scratch copy of the balances, since a later
   leg (the fee) depends on what the earlier one left. *)
let build balances legs =
  let open Or_error.Let_syntax in
  let%map _, rev =
    List.fold_result legs ~init:(balances, []) ~f:(fun (b, acc) leg ->
      let%bind entries = leg b in
      let%map b = Payments_core.Booking.apply b entries in
      b, List.rev_append entries acc)
  in
  List.rev rev
;;

let authorized balances (card : Card.t) (auth : Authorization.t) ~event_id ~at =
  let open Or_error.Let_syntax in
  match auth.status with
  | Pending | Declined _ -> Ok []
  | Authorized | Partially_cleared | Cleared | Reversed | Expired ->
    let%bind hold_id = hold_id auth.id in
    let%bind idempotency_key = key ~event_id "hold" in
    let%map entry =
      Ledger.Hold.place
        balances
        ~hold_id
        ~account:card.account
        ~amount:auth.approved
        ~expires_at:auth.expires_at
        ~idempotency_key
        ~code:Card_auth
        ~reference:(reference_of_auth auth)
        ~effective_at:auth.created_at
        ~recorded_at:at
        ()
    in
    [ entry ]
;;

let incremented balances (auth : Authorization.t) ~amount ~event_id ~at =
  let open Or_error.Let_syntax in
  let%bind hold_id = hold_id auth.id in
  let%bind idempotency_key = key ~event_id "increment" in
  let%map entry =
    Ledger.Hold.increment
      balances
      ~hold_id
      ~amount
      ~idempotency_key
      ~code:Card_auth
      ~reference:(reference_of_auth auth)
      ~effective_at:at
      ~recorded_at:at
      ()
  in
  [ entry ]
;;

let reversed balances (auth : Authorization.t) ~amount ~event_id ~at =
  let open Or_error.Let_syntax in
  let%bind hold_id = hold_id auth.id in
  let%bind idempotency_key = key ~event_id "release" in
  let%map entry =
    Ledger.Hold.release
      balances
      ~hold_id
      ~amount
      ~idempotency_key
      ~code:Reversal
      ~reference:(reference_of_auth auth)
      ~effective_at:at
      ~recorded_at:at
      ()
  in
  [ entry ]
;;

let expire_leg balances hold_id ~event_id ~at =
  let open Or_error.Let_syntax in
  let%bind idempotency_key = key ~event_id "expire" in
  let%map entry =
    Ledger.Hold.expire
      balances
      ~hold_id
      ~idempotency_key
      ~effective_at:at
      ~recorded_at:at
      ()
  in
  [ entry ]
;;

let expired balances (auth : Authorization.t) ~event_id ~at =
  let open Or_error.Let_syntax in
  let%bind hold_id = hold_id auth.id in
  expire_leg balances hold_id ~event_id ~at
;;

(* A debit the cardholder's account pays as far as its available balance goes; the rest
   becomes a receivable. One entry, so the split is visible on one line. *)
let spend_leg
  ?sca
  (chart : Chart.t)
  (card : Card.t)
  ~amount
  ~code
  ~to_
  ~reference
  ~leg
  ~event_id
  ~at
  balances
  =
  let open Or_error.Let_syntax in
  if Amount.is_zero amount
  then Ok []
  else (
    let currency = Amount.currency amount in
    let available = Ledger.Balances.available balances card.account currency in
    let from_account = Amount.max (Amount.zero currency) (Amount.min amount available) in
    let shortfall = Amount.sub_exn amount from_account in
    let%bind idempotency_key = key ~event_id leg in
    let debits =
      List.filter_opt
        [ Option.some_if
            (Amount.is_positive from_account)
            (Ledger.Posting.debit card.account from_account)
        ; Option.some_if
            (Amount.is_positive shortfall)
            (Ledger.Posting.debit (chart.card_receivable currency) shortfall)
        ]
    in
    let%map entry =
      Ledger.Entry.create
        ~idempotency_key
        ~kind:(Transfer "card")
        ~code
        ~reference
        ?sca
        ~effective_at:at
        ~recorded_at:at
        (debits @ [ Ledger.Posting.credit to_ amount ])
    in
    [ entry ])
;;

let fee_leg chart card (clearing : Settlement.Clearing.t) ~event_id =
  match clearing.fx with
  | Some { fee; _ } when Amount.is_positive fee ->
    spend_leg
      chart
      card
      ~amount:fee
      ~code:Fee
      ~to_:(chart.fee_income (Amount.currency fee))
      ~reference:(Ids.Clearing_id.to_string clearing.id)
      ~leg:"fx-fee"
      ~event_id
      ~at:clearing.at
  | Some _ | None -> fun _ -> Ok []
;;

let capture_leg
  (chart : Chart.t)
  (card : Card.t)
  (clearing : Settlement.Clearing.t)
  (auth : Authorization.t)
  hold_id
  (applied : Authorization.Applied.t)
  ~event_id
  balances
  =
  let open Or_error.Let_syntax in
  let currency = Amount.currency clearing.billing in
  let settlement = chart.card_settlement currency in
  let reference = Ids.Clearing_id.to_string clearing.id in
  let at = clearing.at in
  let hold_remaining =
    match Ledger.Balances.find_hold balances hold_id with
    | Some h -> h.remaining
    | None -> Amount.zero currency
  in
  let hold_expired =
    match Ledger.Balances.find_hold balances hold_id with
    | Some { expires_at = Some expires_at; _ } -> Time_ns.( >= ) at expires_at
    | Some { expires_at = None; _ } | None -> false
  in
  let spend amount ~leg =
    (* The hold entries cannot carry SCA evidence (the ledger's hold builders take none),
       so a debit booked directly carries the authorization's. *)
    spend_leg
      ?sca:auth.sca
      chart
      card
      ~amount
      ~code:Card_settlement
      ~to_:settlement
      ~reference
      ~leg
      ~event_id
      ~at
  in
  if Amount.is_positive hold_remaining && hold_expired
  then
    (* The hold ran out before the clearing arrived: give it back, then book the whole
       clearing as new spend. *)
    build
      balances
      [ (fun b -> expire_leg b hold_id ~event_id ~at)
      ; spend clearing.billing ~leg:"settle"
      ]
  else if Amount.is_zero applied.from_hold
  then spend clearing.billing ~leg:"settle" balances
  else if Amount.( > ) applied.from_hold hold_remaining
  then
    Or_error.error_s
      [%message
        "Booking: the authorization and the ledger hold disagree"
          (hold_id : Ledger.Hold_id.t)
          ~from_hold:(applied.from_hold : Amount.t)
          ~ledger_remaining:(hold_remaining : Amount.t)]
  else if Amount.is_zero applied.excess
  then (
    let%bind idempotency_key = key ~event_id "settle" in
    let%map entry =
      Ledger.Hold.capture
        balances
        ~hold_id
        ~amount:applied.from_hold
        ~release_remainder:(Amount.is_positive applied.released)
        ~to_:settlement
        ~idempotency_key
        ~code:Card_settlement
        ~reference
        ~effective_at:at
        ~recorded_at:at
        ()
    in
    [ entry ])
  else (
    let available = Ledger.Balances.available balances card.account currency in
    if Amount.( <= ) applied.excess available
    then (
      let%bind idempotency_key = key ~event_id "settle" in
      let%map entry =
        Ledger.Hold.over_capture
          balances
          ~hold_id
          ~amount:clearing.billing
          ~reason:"clearing above the authorization"
          ~to_:settlement
          ~idempotency_key
          ~code:Card_settlement
          ~reference
          ~effective_at:at
          ~recorded_at:at
          ()
      in
      [ entry ])
    else
      build
        balances
        [ (fun b ->
            let%bind idempotency_key = key ~event_id "settle" in
            let%map entry =
              Ledger.Hold.capture
                b
                ~hold_id
                ~amount:applied.from_hold
                ~to_:settlement
                ~idempotency_key
                ~code:Card_settlement
                ~reference
                ~effective_at:at
                ~recorded_at:at
                ()
            in
            [ entry ])
        ; spend applied.excess ~leg:"settle-excess"
        ])
;;

let credit_leg
  (chart : Chart.t)
  (card : Card.t)
  ~amount
  ~code
  ~reference
  ~leg
  ~event_id
  ~at
  _balances
  =
  let open Or_error.Let_syntax in
  let%bind idempotency_key = key ~event_id leg in
  let%map entry =
    Ledger.Entry.create
      ~idempotency_key
      ~kind:(Transfer "card")
      ~code
      ~reference
      ~effective_at:at
      ~recorded_at:at
      [ Ledger.Posting.debit (chart.card_settlement (Amount.currency amount)) amount
      ; Ledger.Posting.credit card.account amount
      ]
  in
  [ entry ]
;;

let settled
  chart
  balances
  (card : Card.t)
  (clearing : Settlement.Clearing.t)
  outcome
  ~event_id
  =
  let open Or_error.Let_syntax in
  let reference = Ids.Clearing_id.to_string clearing.id in
  let at = clearing.at in
  match (outcome : Settlement.Outcome.t) with
  | Refund _ ->
    build
      balances
      [ credit_leg
          chart
          card
          ~amount:clearing.billing
          ~code:Refund
          ~reference
          ~leg:"refund"
          ~event_id
          ~at
      ]
  | Without_auth ->
    build
      balances
      [ spend_leg
          chart
          card
          ~amount:clearing.billing
          ~code:Card_settlement
          ~to_:(chart.card_settlement (Amount.currency clearing.billing))
          ~reference
          ~leg:"settle"
          ~event_id
          ~at
      ; fee_leg chart card clearing ~event_id
      ]
  | Against_auth { auth; applied; review_excess = _ } ->
    let%bind hold_id = hold_id auth.id in
    build
      balances
      [ capture_leg chart card clearing auth hold_id applied ~event_id
      ; fee_leg chart card clearing ~event_id
      ]
;;

let chargeback_won chart balances card ~dispute_id ~amount ~event_id ~at =
  build
    balances
    [ credit_leg
        chart
        card
        ~amount
        ~code:Refund
        ~reference:(Ids.Dispute_id.to_string dispute_id)
        ~leg:"chargeback"
        ~event_id
        ~at
    ]
;;
