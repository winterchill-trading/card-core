open! Core
open Card_core
open Helpers
module Exemption = Payments_core.Sca.Exemption
module Counters = Payments_core.Sca.Counters

let three_ds ?(eci = Some 5) ?(cavv_present = true) trans_status =
  Some { Three_ds.trans_status; eci; cavv_present }
;;

let request
  ?(entry_mode = Entry_mode.Ecommerce)
  ?(initiator = Card_sca.Initiator.Cardholder)
  ?three_ds
  ?exemption
  ?(cardholder_verified = false)
  amount
  : Card_sca.Request.t
  =
  { entry_mode; initiator; amount; three_ds; exemption; cardholder_verified }
;;

let show ?(policy = Card_sca.Policy.default) ?(counters = Counters.zero) request =
  let outcome = Card_sca.decide policy counters request in
  print_s
    [%sexp
      (outcome : Card_sca.Outcome.t)
    , (Card_sca.Outcome.evidence outcome request : Ledger.Sca_evidence.t option)]
;;

let%expect_test "3-D Secure results" =
  show (request ?three_ds:(three_ds Y) (eur "120.00"));
  show (request ?three_ds:(three_ds N) (eur "120.00"));
  show (request ?three_ds:(three_ds R) (eur "120.00"));
  (* Attempted is not authentication: an exemption or a 1A is still needed. *)
  show (request ?three_ds:(three_ds A ~eci:(Some 6)) (eur "120.00"));
  [%expect
    {|
    (Authenticated ((Challenge 3ds)))
    (Failed ())
    (Failed ())
    (Required ())
    |}]
;;

let%expect_test "low-value remote: EUR 30 each, EUR 100 or 5 payments since the last SCA" =
  show (request ~exemption:Low_value_remote (eur "30.00"));
  show (request ~exemption:Low_value_remote (eur "30.01"));
  let counters =
    List.fold [ "25.00"; "25.00"; "25.00" ] ~init:Counters.zero ~f:(fun c a ->
      Counters.record_remote c ~amount:(eur a))
  in
  show ~counters (request ~exemption:Low_value_remote (eur "25.00"));
  show ~counters (request ~exemption:Low_value_remote (eur "25.01"));
  (* The euro thresholds do not apply to other currencies. *)
  show (request ~exemption:Low_value_remote (usd "5.00"));
  [%expect
    {|
    ((Exempt Low_value_remote) ((Exemption "Art 16 Low_value_remote")))
    (Required ())
    ((Exempt Low_value_remote) ((Exemption "Art 16 Low_value_remote")))
    (Required ())
    (Required ())
    |}]
;;

let%expect_test "contactless: EUR 50 each, EUR 150 or 5 taps; PIN resets" =
  show (request ~entry_mode:Contactless (eur "50.00"));
  show (request ~entry_mode:Contactless (eur "50.01"));
  show (request ~entry_mode:Contactless ~cardholder_verified:true (eur "80.00"));
  let tap counters amount =
    let r = request ~entry_mode:Contactless (eur amount) in
    Card_sca.counters_after counters r (Card_sca.decide Card_sca.Policy.default counters r)
  in
  let counters = List.fold [ "10.00"; "10.00"; "10.00"; "10.00"; "10.00" ] ~init:Counters.zero ~f:tap in
  print_s [%sexp (counters : Counters.t)];
  show ~counters (request ~entry_mode:Contactless (eur "1.00"));
  let pin = request ~entry_mode:Contactless ~cardholder_verified:true (eur "1.00") in
  print_s
    [%sexp
      (Card_sca.counters_after counters pin (Card_sca.decide Card_sca.Policy.default counters pin)
       : Counters.t)];
  [%expect
    {|
    ((Exempt Contactless) ((Exemption "Art 11 Contactless")))
    (Required ())
    (Authenticated ((Challenge cardholder_verified)))
    ((remote_count 0) (remote_sum "0.00 EUR") (contactless_count 5)
     (contactless_sum "50.00 EUR"))
    (Required ())
    ((remote_count 0) (remote_sum "0.00 EUR") (contactless_count 0)
     (contactless_sum "0.00 EUR"))
    |}]
;;

let%expect_test "exemptions that depend on the issuer's policy" =
  let tra = Exemption.Tra { band = Eur_250; score = 12 } in
  show (request ~exemption:tra (eur "200.00"));
  let policy = { Card_sca.Policy.default with tra = true } in
  show ~policy (request ~exemption:tra (eur "200.00"));
  show ~policy (request ~exemption:tra (eur "250.01"));
  show (request ~exemption:Trusted_beneficiary (eur "80.00"));
  show (request ~exemption:Recurring_same_amount_payee (eur "9.99"));
  [%expect
    {|
    (Required ())
    ((Exempt (Tra (band Eur_250) (score 12)))
     ((Exemption "Art 18 (Tra(band Eur_250)(score 12))")))
    (Required ())
    (Required ())
    ((Exempt Recurring_same_amount_payee)
     ((Exemption "Art 14 Recurring_same_amount_payee")))
    |}]
;;

let%expect_test "out of scope" =
  show (request ~initiator:Merchant (eur "300.00"));
  show (request ~entry_mode:Manual (eur "300.00"));
  show (request ~entry_mode:Chip (eur "300.00"));
  show (request ~entry_mode:Chip ~cardholder_verified:true (eur "300.00"));
  [%expect
    {|
    ((Out_of_scope Merchant_initiated)
     ((Exemption "(out_of_scope Merchant_initiated)")))
    ((Out_of_scope Mail_or_telephone_order)
     ((Exemption "(out_of_scope Mail_or_telephone_order)")))
    (Required ())
    (Authenticated ((Challenge cardholder_verified)))
    |}]
;;

let%expect_test "ECI and liability shift" =
  List.iter Three_ds.Scheme.all ~f:(fun scheme ->
    List.iter Three_ds.Trans_status.all ~f:(fun status ->
      let eci = Three_ds.expected_eci scheme status in
      let shift =
        Three_ds.liability_shift scheme { trans_status = status; eci; cavv_present = true }
      in
      print_s
        [%sexp (scheme : Three_ds.Scheme.t), (status : Three_ds.Trans_status.t), (eci : int option), (shift : bool)]));
  [%expect
    {|
    (Visa Y (5) true)
    (Visa A (6) true)
    (Visa I (7) false)
    (Visa N (7) false)
    (Visa U (7) false)
    (Visa R () false)
    (Visa C () false)
    (Visa D () false)
    (Mastercard Y (2) true)
    (Mastercard A (1) true)
    (Mastercard I (2) true)
    (Mastercard N (0) false)
    (Mastercard U (0) false)
    (Mastercard R () false)
    (Mastercard C () false)
    (Mastercard D () false)
    |}]
;;

let%test_unit "an exempt or out-of-scope payment always records why" =
  Quickcheck.test
    (Quickcheck.Generator.tuple4
       Entry_mode.quickcheck_generator
       Card_sca.Initiator.quickcheck_generator
       (Option.quickcheck_generator Three_ds.quickcheck_generator)
       (Money.Amount.quickcheck_generator_in Money.Currency.eur))
    ~f:(fun (entry_mode, initiator, three_ds, amount) ->
      let amount = Money.Amount.abs amount in
      let r = request ~entry_mode ~initiator ?three_ds amount in
      let outcome =
        Card_sca.decide { tra = true; trusted_beneficiary = true; corporate = true } Counters.zero r
      in
      match outcome with
      | Required | Failed -> assert (Option.is_none (Card_sca.Outcome.evidence outcome r))
      | Authenticated | Exempt _ | Out_of_scope _ ->
        assert (Option.is_some (Card_sca.Outcome.evidence outcome r)))
;;
