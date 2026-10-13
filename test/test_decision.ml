open! Core
open Card_core
open Helpers

let show ?risk input =
  let decision, auth = authorize ?risk input in
  print_s
    [%message
      (Response_code.to_string (Decision.response_code decision))
        (decision : Decision.t)
        ~status:(auth.status : Authorization.Status.t)
        ~open_hold:(Authorization.open_hold auth : Money.Amount.t)]
;;

let%expect_test "approve" =
  show (input (request (eur "12.50")));
  [%expect
    {|
    (00
     (decision
      (Approve (amount "12.50 EUR") (partial false) (sca Authenticated)))
     (status Authorized) (open_hold "12.50 EUR"))
    |}]
;;

let%expect_test "decline: insufficient funds" =
  show (input ~available:(eur "10.00") (request (eur "12.50")));
  [%expect
    {|
    (51 (decision (Decline Insufficient_funds))
     (status (Declined Insufficient_funds)) (open_hold "0.00 EUR"))
    |}]
;;

let%expect_test "partial approval when the merchant supports it" =
  show (input ~available:(eur "10.00") (request ~partial_supported:true (eur "12.50")));
  [%expect
    {|
    (00
     (decision (Approve (amount "10.00 EUR") (partial true) (sca Authenticated)))
     (status Authorized) (open_hold "10.00 EUR"))
    |}]
;;

let%expect_test "decline: frozen, blocked, requested, expired card" =
  let card = active_card () in
  show
    (input ~card:(Card.freeze card ~at:start |> Or_error.ok_exn) (request (eur "1.00")));
  show (input ~card:(Card.block card ~at:start |> Or_error.ok_exn) (request (eur "1.00")));
  let requested =
    Card.request
      ~id:card_id
      ~holder:"cust-1"
      ~account
      ~currency:Money.Currency.eur
      ~kind:Physical
      ~at:start
      ()
    |> Or_error.ok_exn
  in
  show (input ~card:requested (request (eur "1.00")));
  show
    (input
       (request
          ~at:(Time_ns.of_string_with_utc_offset "2029-11-01 00:00:00Z")
          (eur "1.00")));
  [%expect
    {|
    (57 (decision (Decline (Card_status Frozen)))
     (status (Declined (Card_status Frozen))) (open_hold "0.00 EUR"))
    (62 (decision (Decline (Card_status Blocked)))
     (status (Declined (Card_status Blocked))) (open_hold "0.00 EUR"))
    (57 (decision (Decline (Card_status Requested)))
     (status (Declined (Card_status Requested))) (open_hold "0.00 EUR"))
    (54 (decision (Decline Card_expired)) (status (Declined Card_expired))
     (open_hold "0.00 EUR"))
    |}]
;;

let%expect_test "decline: limits, controls, SCA, risk" =
  let code input =
    print_endline
      (Response_code.to_string
         (Decision.response_code (Decision.decide input ~now:start)))
  in
  code (input (request (eur "500.01")));
  code
    (input
       (request ~merchant:(merchant ~mcc:Mcc.atm (eur "50.00")) (eur "50.00"))
       ~usage:{ (Limits.Usage.zero Money.Currency.eur) with atm_daily_count = 3 });
  code
    (input
       ~card:
         (Card.set_limits
            (active_card ())
            (Limits.with_online (limits ()) false)
            ~at:start
          |> Or_error.ok_exn)
       (request ~entry_mode:Ecommerce (eur "5.00")));
  code (input (request ~entry_mode:Ecommerce ~cardholder_verified:false (eur "45.00")));
  code
    (input
       (request
          ~entry_mode:Ecommerce
          ~cardholder_verified:false
          ~three_ds:{ trans_status = N; eci = Some 7; cavv_present = false }
          (eur "45.00")));
  code
    (input
       (request
          ~cardholder_verified:false
          ~entry_mode:Ecommerce
          ~exemption:Low_value_remote
          (eur "20.00")));
  let risk (_ : Decision.Input.t) = Decision.Risk.Review "card_testing" in
  print_endline
    (Response_code.to_string
       (Decision.response_code
          (Decision.decide ~risk (input (request (eur "5.00"))) ~now:start)));
  code (input (request (usd "5.00")));
  [%expect {|
    61
    65
    57
    1A
    05
    00
    05
    05
    |}]
;;

let%expect_test "an increment needs an open authorization with the same transaction" =
  let _, hotel =
    authorize
      (input
         (request
            ~estimated:true
            ~merchant:(merchant ~name:"HOTEL" ~mcc:Mcc.lodging (eur "200.00"))
            (eur "200.00")))
  in
  print_s [%sexp (hotel.expires_at : Payments_core.Utc_time.t)];
  let inc =
    request ~auth_id:"auth-2" ~kind:Incremental ~merchant:hotel.merchant (eur "50.00")
  in
  let decision = Decision.decide (input ~open_auth:hotel inc) ~now:start in
  print_s [%sexp (decision : Decision.t)];
  let hotel =
    Authorization.increment hotel inc ~approved:(eur "50.00") |> Or_error.ok_exn
  in
  print_s
    [%sexp
      (hotel.approved : Money.Amount.t)
      , (hotel.increments : Ids.Auth_id.t list)
      , (hotel.expires_at : Payments_core.Utc_time.t)];
  print_s
    [%sexp
      (Authorization.increment hotel inc ~approved:(eur "50.00")
       : Authorization.t Or_error.t)];
  let stray =
    request ~auth_id:"auth-3" ~network_txn_id:"other" ~kind:Incremental (eur "5.00")
  in
  print_s [%sexp (Decision.decide (input ~open_auth:hotel stray) ~now:start : Decision.t)];
  [%expect
    {|
    "2026-11-12 09:00:00Z"
    (Approve (amount "50.00 EUR") (partial false) (sca Authenticated))
    ("250.00 EUR" (auth-2) "2026-11-12 09:00:00Z")
    (Error ("Authorization: increment already applied" (increment auth-2)))
    (Decline Unmatched_increment)
    |}]
;;

let%expect_test "an advice is recorded, not decided" =
  let r = request (eur "80.00") in
  let auth =
    Authorization.advice
      r
      ~approved:(eur "80.00")
      ~response_code:Approved
      ~decided_by:Stand_in
    |> Or_error.ok_exn
  in
  print_s
    [%sexp
      (auth.status : Authorization.Status.t)
      , (auth.decided_by : Authorization.Decided_by.t)];
  let declined =
    Authorization.advice
      r
      ~approved:(eur "0.00")
      ~response_code:Issuer_unavailable
      ~decided_by:Stand_in
    |> Or_error.ok_exn
  in
  print_s [%sexp (declined.status : Authorization.Status.t)];
  (* An advice cannot be declined afterwards: it is not pending. *)
  print_s
    [%sexp
      (Authorization.decline auth Insufficient_funds ~decided_by:Us ~at:start
       : Authorization.t Or_error.t)];
  [%expect
    {|
    (Authorized Stand_in)
    (Declined (Declined_upstream 91))
    (Error
     ("Authorization: not allowed in this state" (auth auth-1)
      (status Authorized)))
    |}]
;;

let%expect_test "hold validity" =
  List.iter
    [ grocery, false
    ; grocery, true
    ; Mcc.lodging, false
    ; Mcc.vehicle_rental, false
    ; Mcc.cruise, false
    ; Mcc.fuel, false
    ]
    ~f:(fun (mcc, estimated) ->
      print_s
        [%sexp
          (mcc : Mcc.t)
          , (estimated : bool)
          , (Authorization.hold_validity ~mcc ~estimated : Time_ns.Span.t)]);
  [%expect
    {|
    (5411 false 7d)
    (5411 true 30d)
    (7011 false 30d)
    (7512 false 30d)
    (4411 false 30d)
    (5542 false 7d)
    |}]
;;
