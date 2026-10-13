open! Core
open Card_core

let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn
let usd s = Money.Amount.of_decimal_string Money.Currency.usd s |> Or_error.ok_exn
let start = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"

(** [at m] is [m] minutes after [start]. *)
let at minutes = Time_ns.add start (Time_ns.Span.of_int_sec (minutes * 60))

let days n = Time_ns.Span.of_day (Float.of_int n)
let account = Ledger.Account_id.of_string "customer:1/eur"
let card_id = Ids.Card_id.of_string "card-1"
let de = Country.of_string "DE"
let grocery = Mcc.of_int_exn 5411

let masked =
  { Masked_pan.last4 = Masked_pan.Last4.of_string "4242"
  ; expiry = Masked_pan.Expiry.create ~month:10 ~year:2029 |> Or_error.ok_exn
  }
;;

let limits () =
  Limits.create
    Money.Currency.eur
    ~per_transaction:(eur "500.00")
    ~daily:(eur "1000.00")
    ~monthly:(eur "3000.00")
    ~atm_daily:(eur "300.00")
    ~atm_daily_count:3
  |> Or_error.ok_exn
;;

let active_card ?(limits = limits ()) () =
  Card.request
    ~id:card_id
    ~holder:"cust-1"
    ~account
    ~currency:Money.Currency.eur
    ~kind:Virtual
    ~limits
    ~at:start
    ()
  |> Or_error.ok_exn
  |> Card.activate ~masked_pan:masked ~at:start
  |> Or_error.ok_exn
;;

let merchant ?(name = "COFFEE BAR BERLIN") ?(mcc = grocery) ?(country = de) amount =
  Merchant.create ~name ~mcc ~country ~amount |> Or_error.ok_exn
;;

let request
  ?(auth_id = "auth-1")
  ?(network_txn_id = "ntx-1")
  ?(kind = Authorization.Kind.Auth)
  ?merchant:m
  ?(entry_mode = Entry_mode.Chip)
  ?(initiator = Card_sca.Initiator.Cardholder)
  ?(estimated = false)
  ?(partial_supported = false)
  ?three_ds
  ?exemption
  ?(cardholder_verified = true)
  ?(at = start)
  amount
  : Authorization.Request.t
  =
  { auth_id = Ids.Auth_id.of_string auth_id
  ; card_id
  ; network_txn_id = Ids.Network_txn_id.of_string network_txn_id
  ; kind
  ; amount
  ; merchant = Option.value m ~default:(merchant amount)
  ; entry_mode
  ; initiator
  ; estimated
  ; partial_supported
  ; three_ds
  ; exemption
  ; cardholder_verified
  ; at
  }
;;

let input ?(card = active_card ()) ?usage ?(available = eur "1000.00") ?open_auth request
  : Decision.Input.t
  =
  { card
  ; usage = Option.value usage ~default:(Limits.Usage.zero Money.Currency.eur)
  ; available
  ; sca_policy = Card_sca.Policy.default
  ; sca_counters = Payments_core.Sca.Counters.zero
  ; request
  ; open_auth
  }
;;

(** Pending, then decided as [Decision.decide] says. *)
let authorize ?risk input =
  let decision = Decision.decide ?risk input ~now:input.Decision.Input.request.at in
  let auth = Authorization.pending input.request |> Or_error.ok_exn in
  decision, Decision.apply decision auth ~request:input.request |> Or_error.ok_exn
;;

let bank = Ledger.Account_id.of_string "bank:central"
let chart = Booking.Chart.default

(** Balances with the card's funding account, a bank account, the chart's accounts, and
    [deposit] paid in. *)
let ledger ?(deposit = eur "100.00") () =
  let accounts =
    Ledger.Account.create ~purpose:Customer_funds account Liability
    :: Ledger.Account.create ~allow_negative:true bank Asset
    :: Booking.Chart.accounts chart ~currencies:[ Money.Currency.eur ]
  in
  let b =
    List.fold accounts ~init:Ledger.Balances.empty ~f:(fun b a ->
      Ledger.Balances.apply b { seq = Ledger.Balances.next_seq b; event = Account_opened a }
      |> Or_error.ok_exn)
  in
  let entry =
    Ledger.Entry.create
      ~idempotency_key:(Ledger.Idempotency_key.of_string "deposit-1")
      ~kind:(Transfer "deposit")
      ~code:Deposit
      ~effective_at:start
      ~recorded_at:start
      [ Ledger.Posting.debit bank deposit; Ledger.Posting.credit account deposit ]
    |> Or_error.ok_exn
  in
  Payments_core.Booking.apply b [ entry ] |> Or_error.ok_exn
;;

let book b entries = Payments_core.Booking.apply b (Or_error.ok_exn entries) |> Or_error.ok_exn

let show_balances b =
  let cur = Money.Currency.eur in
  let line name id f = printf "  %-22s %s\n" name (Money.Amount.to_string (f b id cur)) in
  line "available" account Ledger.Balances.available;
  line "held" account Ledger.Balances.held;
  line "card settlement" (chart.card_settlement cur) Ledger.Balances.balance;
  line "card receivable" (chart.card_receivable cur) Ledger.Balances.balance;
  line "fee income" (chart.fee_income cur) Ledger.Balances.balance
;;

let show_entries entries =
  List.iter (Or_error.ok_exn entries) ~f:(fun (e : Ledger.Entry.t) ->
    printf
      "  %s %s %s\n"
      (Ledger.Idempotency_key.to_string e.idempotency_key)
      (Ledger.Entry.Code.to_string e.code)
      (Sexp.to_string [%sexp (e.kind : Ledger.Entry.Kind.t)]);
    List.iter e.postings ~f:(fun p ->
      printf
        "    %s %-22s %s\n"
        (match p.side with
         | Debit -> "Dr"
         | Credit -> "Cr")
        (Ledger.Account_id.to_string p.account)
        (Money.Amount.to_string p.amount)))
;;

let clearing ?(id = "clr-1") ?(auth_id = Some "auth-1") ?(direction = Settlement.Direction.Debit) ?(final = true) ?merchant:m ?rate ?fx_fee ?(at = at 60) amount =
  Settlement.Clearing.create
    ~id:(Ids.Clearing_id.of_string id)
    ~card_id
    ?auth_id:(Option.map auth_id ~f:Ids.Auth_id.of_string)
    ~direction
    ~merchant:(Option.value m ~default:(merchant amount))
    ~billing_currency:Money.Currency.eur
    ?rate
    ?fx_fee
    ~final
    ~at
    ()
  |> Or_error.ok_exn
;;
