open! Core
open Card_core
open Helpers

let check ?(usage = Limits.Usage.zero Money.Currency.eur) ?(mcc = grocery) ?(entry_mode = Entry_mode.Chip) ?(limits = limits ()) amount =
  print_s
    [%sexp
      (Limits.check limits ~usage ~amount ~mcc ~country:de ~entry_mode
       : (unit, Limits.Violation.t) Result.t Or_error.t)]
;;

let%expect_test "caps" =
  check (eur "500.00");
  check (eur "500.01");
  let usage = Limits.Usage.add (Limits.Usage.zero Money.Currency.eur) (eur "900.00") ~atm:false |> Or_error.ok_exn in
  check ~usage (eur "100.00");
  check ~usage (eur "100.01");
  check (usd "1.00");
  [%expect
    {|
    (Ok (Ok ()))
    (Ok (Error Per_transaction))
    (Ok (Ok ()))
    (Ok (Error Daily))
    (Error
     ("Limits: amount not in the billing currency" (amount "1.00 USD")
      (billing EUR)))
    |}]
;;

let%expect_test "ATM amount and count" =
  let withdraw usage amount = Limits.Usage.add usage amount ~atm:true |> Or_error.ok_exn in
  let usage = withdraw (Limits.Usage.zero Money.Currency.eur) (eur "100.00") in
  check ~usage ~mcc:Mcc.atm (eur "200.01");
  let usage = withdraw (withdraw usage (eur "10.00")) (eur "10.00") in
  check ~usage ~mcc:Mcc.atm (eur "10.00");
  print_s [%sexp (Limits.Violation.response_code Atm_count : Response_code.t)];
  [%expect
    {|
    (Ok (Error Atm_amount))
    (Ok (Error Atm_count))
    65
    |}]
;;

let%expect_test "toggles and block lists" =
  let base = limits () in
  check ~limits:(Limits.with_online base false) ~entry_mode:Ecommerce (eur "1.00");
  check ~limits:(Limits.with_online base false) ~entry_mode:Chip (eur "1.00");
  check ~limits:(Limits.with_contactless base false) ~entry_mode:Contactless (eur "1.00");
  check ~limits:(Limits.with_atm base false) ~mcc:Mcc.atm (eur "1.00");
  let blocked =
    Limits.create Money.Currency.eur ~blocked_mccs:[ Mcc.gambling ] ~blocked_countries:[ Country.of_string "KP" ]
    |> Or_error.ok_exn
  in
  check ~limits:blocked ~mcc:Mcc.gambling (eur "1.00");
  print_s [%sexp (Limits.check blocked ~usage:(Limits.Usage.zero Money.Currency.eur) ~amount:(eur "1.00") ~mcc:grocery ~country:(Country.of_string "KP") ~entry_mode:Chip : (unit, Limits.Violation.t) Result.t Or_error.t)];
  [%expect
    {|
    (Ok (Error Online_disabled))
    (Ok (Ok ()))
    (Ok (Error Contactless_disabled))
    (Ok (Error Atm_disabled))
    (Ok (Error (Mcc_blocked 7995)))
    (Ok (Error (Country_blocked KP)))
    |}]
;;

let%expect_test "creation checks" =
  let show r = print_s [%sexp (r : Limits.t Or_error.t)] in
  show (Limits.create Money.Currency.eur ~daily:(usd "10.00"));
  show (Limits.create Money.Currency.eur ~daily:(eur "0.00"));
  show (Limits.create Money.Currency.eur ~atm_daily_count:0);
  [%expect
    {|
    (Error
     ("Limits: cap in another currency" daily (amount "10.00 USD")
      (currency EUR)))
    (Error ("Limits: cap must be positive" daily (amount "0.00 EUR")))
    (Error ("Limits: ATM count must be positive" (n 0)))
    |}]
;;

let%expect_test "headroom" =
  let usage = Limits.Usage.add (Limits.Usage.zero Money.Currency.eur) (eur "900.00") ~atm:false |> Or_error.ok_exn in
  let show mcc =
    print_s [%sexp (Limits.headroom (limits ()) ~usage ~mcc : Money.Amount.t option Or_error.t)]
  in
  show grocery;
  show Mcc.atm;
  print_s
    [%sexp
      (Limits.headroom (Limits.unlimited Money.Currency.eur) ~usage ~mcc:grocery
       : Money.Amount.t option Or_error.t)];
  [%expect
    {|
    (Ok ("100.00 EUR"))
    (Ok ("100.00 EUR"))
    (Ok ())
    |}]
;;
