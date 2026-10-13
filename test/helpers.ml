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
