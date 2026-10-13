open! Core
open Card_core
open Helpers

(* The standard flows from authorization to clearing, booked in a ledger. Each step prints
   the entries it books and the balances after it. *)

let card = active_card ()

let approve ?(available = eur "100.00") ?partial_supported b amount =
  let _, auth = authorize (input ~available (request ?partial_supported amount)) in
  let entries = Booking.authorized b card auth ~event_id:"evt-auth" ~at:start in
  show_entries entries;
  auth, book b entries
;;

let settle b auth clearing ~event_id =
  let outcome = Settlement.settle clearing ~auth:(Some auth) |> Or_error.ok_exn in
  let entries = Booking.settled chart b card clearing outcome ~event_id in
  show_entries entries;
  outcome, book b entries
;;

let auth_of (outcome : Settlement.Outcome.t) =
  match outcome with
  | Against_auth { auth; _ } -> auth
  | Refund { auth = Some auth } -> auth
  | Refund { auth = None } | Without_auth -> failwith "no authorization"
;;

let%expect_test "approve, then clear in full" =
  let b = ledger () in
  let auth, b = approve b (eur "12.50") in
  show_balances b;
  let outcome, b = settle b auth (clearing (eur "12.50")) ~event_id:"evt-clr" in
  show_balances b;
  print_s [%sexp ((auth_of outcome).status : Authorization.Status.t)];
  [%expect
    {|
      evt-auth/hold CARD_AUTH (Hold_place_expiring(hold card-auth:auth-1)(expires_at"2026-10-20 09:00:00Z"))
        Dr customer:1/eur         12.50 EUR
        Cr customer:1/eur/pending 12.50 EUR
      available              87.50 EUR
      held                   12.50 EUR
      card settlement        0.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
      evt-clr/settle CARD_SETTLEMENT (Hold_capture card-auth:auth-1)
        Dr customer:1/eur/pending 12.50 EUR
        Cr clearing:card/EUR      12.50 EUR
      available              87.50 EUR
      held                   0.00 EUR
      card settlement        -12.50 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
    Cleared
    |}]
;;

let%expect_test "declined for insufficient funds: nothing is booked" =
  let b = ledger ~deposit:(eur "10.00") () in
  let decision, auth =
    authorize (input ~available:(eur "10.00") (request (eur "12.50")))
  in
  print_s [%sexp (decision : Decision.t)];
  show_entries (Booking.authorized b card auth ~event_id:"evt-auth" ~at:start);
  [%expect {| (Decline Insufficient_funds) |}]
;;

let%expect_test "partial settlement: a split shipment cleared in two parts" =
  let b = ledger () in
  let auth, b = approve b (eur "60.00") in
  let outcome, b =
    settle b auth (clearing ~id:"clr-1" ~final:false (eur "25.00")) ~event_id:"evt-clr-1"
  in
  let auth = auth_of outcome in
  print_s
    [%sexp
      (auth.status : Authorization.Status.t)
      , (Authorization.open_hold auth : Money.Amount.t)];
  show_balances b;
  (* The second and last part is smaller than what is left: the rest is released in the
     same entry. *)
  let outcome, b =
    settle b auth (clearing ~id:"clr-2" (eur "30.00")) ~event_id:"evt-clr-2"
  in
  let auth = auth_of outcome in
  print_s
    [%sexp
      (auth.status : Authorization.Status.t)
      , (auth.captured : Money.Amount.t)
      , (auth.released : Money.Amount.t)];
  show_balances b;
  [%expect
    {|
      evt-auth/hold CARD_AUTH (Hold_place_expiring(hold card-auth:auth-1)(expires_at"2026-10-20 09:00:00Z"))
        Dr customer:1/eur         60.00 EUR
        Cr customer:1/eur/pending 60.00 EUR
      evt-clr-1/settle CARD_SETTLEMENT (Hold_capture card-auth:auth-1)
        Dr customer:1/eur/pending 25.00 EUR
        Cr clearing:card/EUR      25.00 EUR
    (Partially_cleared "35.00 EUR")
      available              40.00 EUR
      held                   35.00 EUR
      card settlement        -25.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
      evt-clr-2/settle CARD_SETTLEMENT (Hold_capture card-auth:auth-1)
        Dr customer:1/eur/pending 35.00 EUR
        Cr customer:1/eur         5.00 EUR
        Cr clearing:card/EUR      30.00 EUR
    (Cleared "55.00 EUR" "5.00 EUR")
      available              45.00 EUR
      held                   0.00 EUR
      card settlement        -55.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
    |}]
;;

let%expect_test "reversal: partial, then the rest" =
  let b = ledger () in
  let auth, b = approve b (eur "40.00") in
  let auth, released =
    Authorization.reverse auth ~amount:(eur "15.00") ~at:(at 5) () |> Or_error.ok_exn
  in
  let b =
    book b (Booking.reversed b auth ~amount:released ~event_id:"evt-rev-1" ~at:(at 5))
  in
  print_s
    [%sexp
      (auth.status : Authorization.Status.t)
      , (Authorization.open_hold auth : Money.Amount.t)];
  show_balances b;
  let auth, released = Authorization.reverse auth ~at:(at 6) () |> Or_error.ok_exn in
  let entries =
    Booking.reversed b auth ~amount:released ~event_id:"evt-rev-2" ~at:(at 6)
  in
  show_entries entries;
  let b = book b entries in
  print_s [%sexp (auth.status : Authorization.Status.t)];
  show_balances b;
  [%expect
    {|
      evt-auth/hold CARD_AUTH (Hold_place_expiring(hold card-auth:auth-1)(expires_at"2026-10-20 09:00:00Z"))
        Dr customer:1/eur         40.00 EUR
        Cr customer:1/eur/pending 40.00 EUR
    (Authorized "25.00 EUR")
      available              75.00 EUR
      held                   25.00 EUR
      card settlement        0.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
      evt-rev-2/release REVERSAL (Hold_release card-auth:auth-1)
        Dr customer:1/eur/pending 25.00 EUR
        Cr customer:1/eur         25.00 EUR
    Reversed
      available              100.00 EUR
      held                   0.00 EUR
      card settlement        0.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
    |}]
;;

let%expect_test "refund: a new credit weeks later, the hold is not touched" =
  let b = ledger () in
  let auth, b = approve b (eur "30.00") in
  let outcome, b = settle b auth (clearing (eur "30.00")) ~event_id:"evt-clr" in
  let auth = auth_of outcome in
  let refund =
    clearing ~id:"clr-refund" ~direction:Credit ~at:(at (60 * 24 * 20)) (eur "30.00")
  in
  let outcome, b = settle b auth refund ~event_id:"evt-refund" in
  let auth = auth_of outcome in
  print_s [%sexp (auth.status : Authorization.Status.t), (auth.refunded : Money.Amount.t)];
  show_balances b;
  [%expect
    {|
      evt-auth/hold CARD_AUTH (Hold_place_expiring(hold card-auth:auth-1)(expires_at"2026-10-20 09:00:00Z"))
        Dr customer:1/eur         30.00 EUR
        Cr customer:1/eur/pending 30.00 EUR
      evt-clr/settle CARD_SETTLEMENT (Hold_capture card-auth:auth-1)
        Dr customer:1/eur/pending 30.00 EUR
        Cr clearing:card/EUR      30.00 EUR
      evt-refund/refund REFUND (Transfer card)
        Dr clearing:card/EUR      30.00 EUR
        Cr customer:1/eur         30.00 EUR
    (Cleared "30.00 EUR")
      available              100.00 EUR
      held                   0.00 EUR
      card settlement        0.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
    |}]
;;

let%expect_test "over-capture: a restaurant tip above the authorization" =
  let b = ledger () in
  let auth, b = approve b (eur "40.00") in
  let outcome, b = settle b auth (clearing (eur "46.00")) ~event_id:"evt-clr" in
  (match outcome with
   | Against_auth { applied; review_excess; _ } ->
     print_s [%sexp (applied : Authorization.Applied.t), (review_excess : bool)]
   | _ -> ());
  show_balances b;
  [%expect
    {|
      evt-auth/hold CARD_AUTH (Hold_place_expiring(hold card-auth:auth-1)(expires_at"2026-10-20 09:00:00Z"))
        Dr customer:1/eur         40.00 EUR
        Cr customer:1/eur/pending 40.00 EUR
      evt-clr/settle CARD_SETTLEMENT (Hold_over_capture(hold card-auth:auth-1)(reason"clearing above the authorization"))
        Dr customer:1/eur/pending 40.00 EUR
        Dr customer:1/eur         6.00 EUR
        Cr clearing:card/EUR      46.00 EUR
    (((from_hold "40.00 EUR") (excess "6.00 EUR") (released "0.00 EUR")) false)
      available              54.00 EUR
      held                   0.00 EUR
      card settlement        -46.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
    |}]
;;

let%expect_test "over-capture the account cannot pay: the shortfall is a receivable" =
  let b = ledger ~deposit:(eur "50.00") () in
  let auth, b = approve ~available:(eur "50.00") b (eur "50.00") in
  let outcome, b = settle b auth (clearing (eur "65.00")) ~event_id:"evt-clr" in
  (match outcome with
   | Against_auth { review_excess; _ } -> print_s [%sexp (review_excess : bool)]
   | _ -> ());
  show_balances b;
  [%expect
    {|
      evt-auth/hold CARD_AUTH (Hold_place_expiring(hold card-auth:auth-1)(expires_at"2026-10-20 09:00:00Z"))
        Dr customer:1/eur         50.00 EUR
        Cr customer:1/eur/pending 50.00 EUR
      evt-clr/settle CARD_SETTLEMENT (Hold_capture card-auth:auth-1)
        Dr customer:1/eur/pending 50.00 EUR
        Cr clearing:card/EUR      50.00 EUR
      evt-clr/settle-excess CARD_SETTLEMENT (Transfer card)
        Dr receivable:card/EUR    15.00 EUR
        Cr clearing:card/EUR      15.00 EUR
    true
      available              0.00 EUR
      held                   0.00 EUR
      card settlement        -65.00 EUR
      card receivable        15.00 EUR
      fee income             0.00 EUR
    |}]
;;

let%expect_test "expiry, then a late clearing still posts" =
  let b = ledger () in
  let auth, b = approve b (eur "20.00") in
  let later = Time_ns.add start (days 8) in
  let auth, released = Authorization.expire auth ~now:later |> Or_error.ok_exn in
  let entries = Booking.expired b auth ~event_id:"evt-exp" ~at:later in
  show_entries entries;
  let b = book b entries in
  print_s [%sexp (auth.status : Authorization.Status.t), (released : Money.Amount.t)];
  let outcome, b =
    settle
      b
      auth
      (clearing ~at:(Time_ns.add later (days 1)) (eur "20.00"))
      ~event_id:"evt-clr"
  in
  print_s [%sexp ((auth_of outcome).status : Authorization.Status.t)];
  show_balances b;
  [%expect
    {|
      evt-auth/hold CARD_AUTH (Hold_place_expiring(hold card-auth:auth-1)(expires_at"2026-10-20 09:00:00Z"))
        Dr customer:1/eur         20.00 EUR
        Cr customer:1/eur/pending 20.00 EUR
      evt-exp/expire HOLD (Hold_expire card-auth:auth-1)
        Dr customer:1/eur/pending 20.00 EUR
        Cr customer:1/eur         20.00 EUR
    (Expired "20.00 EUR")
      evt-clr/settle CARD_SETTLEMENT (Transfer card)
        Dr customer:1/eur         20.00 EUR
        Cr clearing:card/EUR      20.00 EUR
    Cleared
      available              80.00 EUR
      held                   0.00 EUR
      card settlement        -20.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
    |}]
;;

let%expect_test "a clearing that arrives after the hold lapsed, before anyone expired it" =
  let b = ledger () in
  let auth, b = approve b (eur "20.00") in
  let outcome, b =
    settle
      b
      auth
      (clearing ~at:(Time_ns.add start (days 9)) (eur "20.00"))
      ~event_id:"evt-clr"
  in
  print_s [%sexp ((auth_of outcome).status : Authorization.Status.t)];
  show_balances b;
  [%expect
    {|
      evt-auth/hold CARD_AUTH (Hold_place_expiring(hold card-auth:auth-1)(expires_at"2026-10-20 09:00:00Z"))
        Dr customer:1/eur         20.00 EUR
        Cr customer:1/eur/pending 20.00 EUR
      evt-clr/expire HOLD (Hold_expire card-auth:auth-1)
        Dr customer:1/eur/pending 20.00 EUR
        Cr customer:1/eur         20.00 EUR
      evt-clr/settle CARD_SETTLEMENT (Transfer card)
        Dr customer:1/eur         20.00 EUR
        Cr clearing:card/EUR      20.00 EUR
    Cleared
      available              80.00 EUR
      held                   0.00 EUR
      card settlement        -20.00 EUR
      card receivable        0.00 EUR
      fee income             0.00 EUR
    |}]
;;

let%expect_test "a clearing with no authorization, in another currency, with our FX fee" =
  let b = ledger () in
  let rate =
    Money.Rate.create
      ~base:Money.Currency.usd
      ~quote:Money.Currency.eur
      ~price:(Money.Rate.price_of_string "0.92" |> Or_error.ok_exn)
      ~at:start
      ~source:"scheme"
    |> Or_error.ok_exn
  in
  let fx_fee = Money.Fee.create Money.Currency.eur ~bps:100 |> Or_error.ok_exn in
  let c =
    clearing
      ~auth_id:None
      ~merchant:
        (merchant ~name:"BOOKSHOP NYC" ~country:(Country.of_string "US") (usd "25.00"))
      ~rate
      ~fx_fee
      (usd "25.00")
  in
  print_s
    [%sexp
      (c.billing : Money.Amount.t), (Settlement.Clearing.total_charged c : Money.Amount.t)];
  let outcome = Settlement.settle c ~auth:None |> Or_error.ok_exn in
  let entries = Booking.settled chart b card c outcome ~event_id:"evt-clr" in
  show_entries entries;
  show_balances (book b entries);
  [%expect
    {|
    ("23.00 EUR" "23.23 EUR")
      evt-clr/settle CARD_SETTLEMENT (Transfer card)
        Dr customer:1/eur         23.00 EUR
        Cr clearing:card/EUR      23.00 EUR
      evt-clr/fx-fee FEE (Transfer card)
        Dr customer:1/eur         0.23 EUR
        Cr income:fees/EUR        0.23 EUR
      available              76.77 EUR
      held                   0.00 EUR
      card settlement        -23.00 EUR
      card receivable        0.00 EUR
      fee income             0.23 EUR
    |}]
;;

let%expect_test "a webhook delivered twice is booked once" =
  let dir = Filename_unix.temp_dir "card-core" "" in
  let journal =
    Ledger.Journal.create ~fsync:false (dir ^/ "journal") |> Or_error.ok_exn
  in
  let b0 = ledger () in
  (* Replay the opening records into the journal. *)
  List.iter (Ledger.Balances.accounts b0) ~f:(fun a ->
    ignore
      (Ledger.Journal.open_account journal a |> Or_error.ok_exn
       : Ledger.Journal.Append_result.t));
  let deposit =
    Ledger.Balances.find_entry b0 (Ledger.Idempotency_key.of_string "deposit-1")
  in
  Option.iter deposit ~f:(fun (_, e) ->
    ignore
      (Ledger.Journal.append_entry journal e |> Or_error.ok_exn
       : Ledger.Journal.Append_result.t));
  let _, auth = authorize (input (request (eur "10.00"))) in
  let deliver () =
    let entries =
      Booking.authorized
        (Ledger.Journal.balances journal)
        card
        auth
        ~event_id:"evt-auth"
        ~at:start
    in
    match entries with
    | Error e -> print_s [%sexp (e : Error.t)]
    | Ok entries ->
      (match Payments_core.Booking.post journal entries |> Or_error.ok_exn with
       | Appended _ -> print_endline "appended"
       | Duplicate _ -> print_endline "duplicate")
  in
  deliver ();
  deliver ();
  Ledger.Journal.close journal;
  [%expect {|
    appended
    duplicate
    |}]
;;
