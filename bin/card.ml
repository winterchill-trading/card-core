open! Core
open Card_core

let codes =
  Command.basic
    ~summary:"List authorization response codes with their retry category"
    (Command.Param.return (fun () ->
       List.iter Response_code.all_known ~f:(fun code ->
         print_endline
         @@ String.rstrip
         @@ sprintf
              "%s  %-20s %s"
              (Response_code.to_string code)
              (match Response_code.category code with
               | None -> "approved"
               | Some c -> Sexp.to_string [%sexp (c : Response_code.Category.t)])
              (if Response_code.is_approved code
               then ""
               else if Response_code.retry_allowed code
               then "retry allowed"
               else "do not retry"))))
;;

let hold_expiry =
  Command.basic
    ~summary:"How long an authorization hold stays valid for a merchant category"
    (let%map_open.Command mcc =
       flag "-mcc" (required string) ~doc:"MCC four-digit category"
     and estimated = flag "-estimated" no_arg ~doc:" the amount is an estimate" in
     fun () ->
       let mcc = Mcc.of_string_or_error mcc |> Or_error.ok_exn in
       print_endline
         (Time_ns.Span.to_string_hum (Authorization.hold_validity ~mcc ~estimated)))
;;

(* A fixed scenario on a fixed clock, so the output is the same on every run. *)
let demo () =
  let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn in
  let start = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z" in
  let at hours = Time_ns.add start (Time_ns.Span.of_hr (Float.of_int hours)) in
  let account = Ledger.Account_id.of_string "customer:demo/eur" in
  let chart = Booking.Chart.default in
  let card =
    Card.request
      ~id:(Ids.Card_id.of_string "card-demo")
      ~holder:"cust-demo"
      ~account
      ~currency:Money.Currency.eur
      ~kind:Virtual
      ~limits:
        (Limits.create
           Money.Currency.eur
           ~per_transaction:(eur "500.00")
           ~daily:(eur "800.00")
         |> Or_error.ok_exn)
      ~at:start
      ()
    |> Or_error.ok_exn
    |> Card.activate
         ~masked_pan:
           { last4 = Masked_pan.Last4.of_string "4242"
           ; expiry = Masked_pan.Expiry.create ~month:10 ~year:2029 |> Or_error.ok_exn
           }
         ~at:start
    |> Or_error.ok_exn
  in
  let open_ b a =
    Ledger.Balances.apply b { seq = Ledger.Balances.next_seq b; event = Account_opened a }
    |> Or_error.ok_exn
  in
  let bank = Ledger.Account_id.of_string "bank:demo" in
  let b =
    List.fold
      (Ledger.Account.create ~purpose:Customer_funds account Liability
       :: Ledger.Account.create ~allow_negative:true bank Asset
       :: Booking.Chart.accounts chart ~currencies:[ Money.Currency.eur ])
      ~init:Ledger.Balances.empty
      ~f:open_
  in
  let book b entries =
    let entries = Or_error.ok_exn entries in
    List.iter entries ~f:(fun (e : Ledger.Entry.t) ->
      printf
        "  book %-24s %-16s %s\n"
        (Ledger.Idempotency_key.to_string e.idempotency_key)
        (Ledger.Entry.Code.to_string e.code)
        (Money.Amount.to_string (Ledger.Entry.total e)));
    Payments_core.Booking.apply b entries |> Or_error.ok_exn
  in
  let b =
    book
      b
      (Ledger.Entry.create
         ~idempotency_key:(Ledger.Idempotency_key.of_string "evt-deposit")
         ~kind:(Transfer "deposit")
         ~code:Deposit
         ~effective_at:start
         ~recorded_at:start
         [ Ledger.Posting.debit bank (eur "200.00")
         ; Ledger.Posting.credit account (eur "200.00")
         ]
       |> Or_error.map ~f:List.return)
  in
  let show_balance b =
    printf
      "  available %s, held %s\n"
      (Money.Amount.to_string (Ledger.Balances.available b account Money.Currency.eur))
      (Money.Amount.to_string (Ledger.Balances.held b account Money.Currency.eur))
  in
  let authorize b ~id ~name ~mcc amount ~hours =
    let merchant =
      Merchant.create
        ~name
        ~mcc:(Mcc.of_int_exn mcc)
        ~country:(Country.of_string "DE")
        ~amount
      |> Or_error.ok_exn
    in
    let request : Authorization.Request.t =
      { auth_id = Ids.Auth_id.of_string id
      ; card_id = card.id
      ; network_txn_id = Ids.Network_txn_id.of_string ("ntx-" ^ id)
      ; kind = Auth
      ; amount
      ; merchant
      ; entry_mode = Contactless
      ; initiator = Cardholder
      ; estimated = false
      ; partial_supported = false
      ; three_ds = None
      ; exemption = None
      ; cardholder_verified = false
      ; at = at hours
      }
    in
    let input : Decision.Input.t =
      { card
      ; usage = Limits.Usage.zero Money.Currency.eur
      ; available = Ledger.Balances.available b account Money.Currency.eur
      ; sca_policy = Card_sca.Policy.default
      ; sca_counters = Payments_core.Sca.Counters.zero
      ; request
      ; open_auth = None
      }
    in
    let decision = Decision.decide input ~now:request.at in
    let auth =
      Decision.apply decision (Authorization.pending request |> Or_error.ok_exn) ~request
      |> Or_error.ok_exn
    in
    printf
      "%s %s %s -> %s %s\n"
      id
      name
      (Money.Amount.to_string amount)
      (Response_code.to_string (Decision.response_code decision))
      (Sexp.to_string [%sexp (decision : Decision.t)]);
    let b =
      book b (Booking.authorized b card auth ~event_id:("evt-" ^ id) ~at:request.at)
    in
    show_balance b;
    auth, b
  in
  let clear
    b
    auth
    ~id
    ?(direction = Settlement.Direction.Debit)
    ?(final = true)
    amount
    ~hours
    =
    let clearing =
      Settlement.Clearing.create
        ~id:(Ids.Clearing_id.of_string id)
        ~card_id:card.id
        ~auth_id:auth.Authorization.id
        ~direction
        ~merchant:(Merchant.with_amount auth.merchant amount |> Or_error.ok_exn)
        ~billing_currency:Money.Currency.eur
        ~final
        ~at:(at hours)
        ()
      |> Or_error.ok_exn
    in
    let outcome = Settlement.settle clearing ~auth:(Some auth) |> Or_error.ok_exn in
    printf
      "%s %s %s\n"
      id
      (Sexp.to_string [%sexp (direction : Settlement.Direction.t)])
      (Money.Amount.to_string amount);
    let b =
      book b (Booking.settled chart b card clearing outcome ~event_id:("evt-" ^ id))
    in
    show_balance b;
    let auth =
      match outcome with
      | Against_auth { auth; _ } | Refund { auth = Some auth } -> auth
      | Refund { auth = None } | Without_auth -> auth
    in
    clearing, auth, b
  in
  let coffee, b =
    authorize b ~id:"a1" ~name:"COFFEE BAR" ~mcc:5814 (eur "4.20") ~hours:1
  in
  let dinner, b =
    authorize b ~id:"a2" ~name:"RESTAURANT" ~mcc:5812 (eur "40.00") ~hours:10
  in
  let _, b =
    authorize b ~id:"a3" ~name:"ELECTRONICS" ~mcc:5732 (eur "600.00") ~hours:11
  in
  let c1, coffee, b = clear b coffee ~id:"c1" (eur "4.20") ~hours:24 in
  let c2, dinner, b = clear b dinner ~id:"c2" (eur "46.00") ~hours:25 in
  let c3, coffee, b = clear b coffee ~id:"c3" ~direction:Credit (eur "4.20") ~hours:200 in
  ignore (b : Ledger.Balances.t);
  let statement =
    Statement.create
      ~card
      ~authorizations:[ coffee; dinner ]
      ~clearings:[ c1; c2; c3 ]
      ~from:start
      ~until:(Time_ns.add start (Time_ns.Span.of_day 30.))
      ()
    |> Or_error.ok_exn
  in
  print_string (Statement.to_string_hum statement)
;;

let demo =
  Command.basic
    ~summary:"Run a fixed scenario: authorizations, clearings, a tip and a refund"
    (Command.Param.return demo)
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Card lifecycle, authorization and clearing tools"
       [ "codes", codes; "hold-expiry", hold_expiry; "demo", demo ])
;;
