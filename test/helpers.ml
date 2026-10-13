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
