open! Core
open Card_core
open Helpers

(* The README's usage example, kept compiling and run once. *)

let answer ~card ~authorizations ~balances (request : Authorization.Request.t) =
  let open Or_error.Let_syntax in
  let%bind usage =
    Authorization.usage authorizations ~currency:card.Card.currency ~now:request.at
  in
  let input : Decision.Input.t =
    { card
    ; usage
    ; available = Ledger.Balances.available balances card.account card.currency
    ; sca_policy = Card_sca.Policy.default
    ; sca_counters =
        Payments_core.Sca.Counters.zero (* the cardholder's, from your store *)
    ; request
    ; open_auth = None
    }
  in
  let decision = Decision.decide input ~now:request.at in
  let%bind auth = Authorization.pending request in
  let%bind auth = Decision.apply decision auth ~request in
  let%map entries =
    Booking.authorized balances card auth ~event_id:"provider-event-1" ~at:request.at
  in
  (* Reply with this code, store [auth], append [entries] to the journal. *)
  Decision.response_code decision, auth, entries
;;

let clear ~card ~auth ~balances (clearing : Settlement.Clearing.t) =
  let open Or_error.Let_syntax in
  let%bind outcome = Settlement.settle clearing ~auth:(Some auth) in
  Booking.settled
    Booking.Chart.default
    balances
    card
    clearing
    outcome
    ~event_id:"provider-event-2"
;;

let%expect_test "README example" =
  let card = active_card () in
  let balances = ledger () in
  let code, auth, entries =
    answer ~card ~authorizations:[] ~balances (request (eur "12.00")) |> Or_error.ok_exn
  in
  let balances = Payments_core.Booking.apply balances entries |> Or_error.ok_exn in
  let entries = clear ~card ~auth ~balances (clearing (eur "12.00")) |> Or_error.ok_exn in
  print_s [%sexp (code : Response_code.t), (List.length entries : int)];
  [%expect {| (00 1) |}]
;;
