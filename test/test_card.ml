open! Core
open Card_core
open Helpers

let%expect_test "transition table" =
  List.iter Card.Status.transitions ~f:(fun (from, tos) ->
    print_s [%sexp (from : Card.Status.t), "->", (tos : Card.Status.t list)]);
  [%expect
    {|
    (Requested -> (Active Destroyed))
    (Active -> (Frozen Blocked Expired Destroyed))
    (Frozen -> (Active Blocked Expired Destroyed))
    (Blocked -> (Destroyed))
    (Expired -> (Destroyed))
    (Destroyed -> ())
    |}]
;;

let%expect_test "decline code per status" =
  List.iter Card.Status.all ~f:(fun status ->
    print_s
      [%sexp
        (status : Card.Status.t)
        , (Card.Status.decline_code status : Response_code.t option)]);
  [%expect
    {|
    (Requested (57))
    (Active ())
    (Frozen (57))
    (Blocked (62))
    (Expired (54))
    (Destroyed (46))
    |}]
;;

let%expect_test "freeze, unfreeze, block; no way back from blocked" =
  let card = active_card () in
  let card = Card.freeze card ~at:(at 1) |> Or_error.ok_exn in
  print_s [%sexp (Card.unusable card ~now:(at 2) : Response_code.t option)];
  (* A repeated freeze (a webhook delivered twice) is harmless. *)
  let card = Card.freeze card ~at:(at 2) |> Or_error.ok_exn in
  let card = Card.unfreeze card ~at:(at 3) |> Or_error.ok_exn in
  print_s [%sexp (Card.unusable card ~now:(at 4) : Response_code.t option)];
  let card = Card.block card ~at:(at 5) |> Or_error.ok_exn in
  print_s [%sexp (Card.unfreeze card ~at:(at 6) : Card.t Or_error.t)];
  [%expect
    {|
    (57)
    ()
    (Error
     ("Card: transition not allowed" (card card-1) (from Blocked) (to_ Active)))
    |}]
;;

let%expect_test "an active card past its expiry date declines with 54" =
  let card = active_card () in
  let after = Time_ns.of_string_with_utc_offset "2029-11-01 00:00:00Z" in
  print_s [%sexp (Card.unusable card ~now:after : Response_code.t option)];
  print_s [%sexp ((Card.expire_if_due card ~now:after).status : Card.Status.t)];
  [%expect {|
    (54)
    Expired
    |}]
;;

let%expect_test "a card as stored" =
  print_s [%sexp (active_card () : Card.t)];
  [%expect
    {|
    ((id card-1) (holder cust-1) (account customer:1/eur) (currency EUR)
     (kind Virtual) (status Active)
     (masked_pan (((last4 4242) (expiry ((month 10) (year 2029))))))
     (limits
      ((currency EUR) (per_transaction ("500.00 EUR")) (daily ("1000.00 EUR"))
       (monthly ("3000.00 EUR")) (atm_daily ("300.00 EUR")) (atm_daily_count (3))
       (online true) (contactless true) (atm true) (blocked_mccs ())
       (blocked_countries ())))
     (created_at "2026-10-13 09:00:00Z") (updated_at "2026-10-13 09:00:00Z"))
    |}]
;;

let%test_unit "every accepted transition is in the table; terminal states stay put" =
  Quickcheck.test
    (List.quickcheck_generator Card.Status.quickcheck_generator)
    ~f:(fun targets ->
      ignore
        (List.fold targets ~init:(active_card ()) ~f:(fun card to_ ->
           match Card.transition card ~to_ ~at:(at 1) with
           | Error _ ->
             assert (not (Card.Status.can_transition ~from:card.status ~to_));
             card
           | Ok next ->
             assert (
               Card.Status.equal next.status card.status
               || Card.Status.can_transition ~from:card.status ~to_);
             if Card.Status.is_terminal card.status
             then assert (Card.Status.equal next.status card.status);
             next)
         : Card.t))
;;
