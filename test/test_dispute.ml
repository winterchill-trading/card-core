open! Core
open Card_core
open Helpers

let cleared = clearing ~at:start (eur "80.00")
let id = Ids.Dispute_id.of_string "dsp-1"

let open_ ?three_ds ?(at = at 60) ?(amount = eur "80.00") reason =
  Dispute.open_ ~id ~clearing:cleared ?three_ds ~scheme:Visa ~reason ~amount ~at ()
;;

let%expect_test "reasons and categories" =
  List.iter Dispute.Reason.all ~f:(fun r ->
    printf "%s %d %s\n" (Dispute.Reason.code r) (Dispute.Category.number (Dispute.Reason.category r))
      (Sexp.to_string [%sexp (r : Dispute.Reason.t)]));
  [%expect
    {|
    10.4 10 Card_absent_fraud
    11.2 11 Declined_authorization
    11.3 11 No_authorization
    12.5 12 Incorrect_amount
    12.6 12 Duplicate_processing
    13.1 13 Not_received
    13.6 13 Credit_not_processed
    |}]
;;

let%expect_test "opening checks" =
  let show r = print_s [%sexp (Result.map r ~f:(fun (d : Dispute.t) -> d.status) : Dispute.Status.t Or_error.t)] in
  show (open_ Not_received);
  show (open_ ~at:(Time_ns.add start (days 121)) Not_received);
  show (open_ ~amount:(eur "80.01") Incorrect_amount);
  let authenticated = { Three_ds.trans_status = Y; eci = Some 5; cavv_present = true } in
  show (open_ ~three_ds:authenticated Card_absent_fraud);
  (* Liability shift only blocks fraud disputes; a parcel that never came is still
     the merchant's problem. *)
  show (open_ ~three_ds:authenticated Not_received);
  [%expect
    {|
    (Ok (Open Filed))
    (Error ("Dispute: filing window closed" (dispute dsp-1)))
    (Error ("Dispute: amount above the cleared amount" (dispute dsp-1)))
    (Error ("Dispute: 3-D Secure moved liability to the issuer" (dispute dsp-1)))
    (Ok (Open Filed))
    |}]
;;

let%expect_test "stages: consumer dispute goes through the merchant's response" =
  let d = open_ Not_received |> Or_error.ok_exn in
  let d = Dispute.add_evidence d ~kind:"cardholder_letter" ~description:"parcel never arrived" ~at:(at 61) |> Or_error.ok_exn in
  print_s [%sexp (Dispute.deadline d : Time_ns.Alternate_sexp.t option)];
  let d = Dispute.advance d ~to_:Response ~at:(Time_ns.add start (days 10)) |> Or_error.ok_exn in
  print_s [%sexp (Dispute.deadline d : Time_ns.Alternate_sexp.t option)];
  let d = Dispute.advance d ~to_:Pre_arbitration ~at:(Time_ns.add start (days 20)) |> Or_error.ok_exn in
  let d = Dispute.advance d ~to_:Pre_arbitration_response ~at:(Time_ns.add start (days 25)) |> Or_error.ok_exn in
  let d = Dispute.advance d ~to_:Arbitration ~at:(Time_ns.add start (days 30)) |> Or_error.ok_exn in
  print_s [%sexp (Dispute.deadline d : Time_ns.Alternate_sexp.t option)];
  let d = Dispute.close d Won ~at:(Time_ns.add start (days 35)) |> Or_error.ok_exn in
  print_s [%sexp (d.status : Dispute.Status.t), (Dispute.deadline d : Time_ns.Alternate_sexp.t option)];
  print_s [%sexp (Dispute.advance d ~to_:Arbitration ~at:start : Dispute.t Or_error.t)];
  [%expect
    {|
    ("2026-11-12 10:00:00Z")
    ("2026-11-22 09:00:00Z")
    ("2026-11-22 09:00:00Z")
    ((Closed Won) ())
    (Error ("Dispute: closed" (dispute dsp-1)))
    |}]
;;

let%expect_test "fraud disputes skip the response stage" =
  let d = open_ Card_absent_fraud |> Or_error.ok_exn in
  print_s [%sexp (Dispute.advance d ~to_:Response ~at:(at 70) : Dispute.t Or_error.t)];
  print_s [%sexp (Result.map (Dispute.advance d ~to_:Pre_arbitration ~at:(at 70)) ~f:(fun d -> d.status) : Dispute.Status.t Or_error.t)];
  print_s [%sexp (Dispute.add_evidence d ~kind:"note" ~description:"used 4111111111111111" ~at:(at 70) : Dispute.t Or_error.t)];
  [%expect
    {|
    (Error
     ("Dispute: stage not allowed" (dispute dsp-1) (from Filed) (to_ Response)
      (reason 10.4)))
    (Ok (Open Pre_arbitration))
    (Error ("text looks like it contains a card number" "evidence description"))
    |}]
;;

let%expect_test "a won chargeback is credited back" =
  let b = ledger () in
  let card = active_card () in
  let d = open_ Not_received |> Or_error.ok_exn in
  let entries = Booking.chargeback_won chart b card ~dispute_id:d.id ~amount:d.amount ~event_id:"evt-dsp" ~at:(at 90) in
  show_entries entries;
  [%expect
    {|
      evt-dsp/chargeback REFUND (Transfer card)
        Dr clearing:card/EUR      80.00 EUR
        Cr customer:1/eur         80.00 EUR
    |}]
;;
