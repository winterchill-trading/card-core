open! Core
open Card_core
open Helpers

let%expect_test "a month on one card" =
  let card = active_card () in
  let auth amount ~id ~merchant:m ~at:minutes =
    let r =
      request
        ~auth_id:id
        ~network_txn_id:("ntx-" ^ id)
        ~merchant:m
        ~at:(at minutes)
        amount
    in
    snd (authorize (input r))
  in
  let coffee = auth (eur "3.80") ~id:"a1" ~merchant:(merchant (eur "3.80")) ~at:10 in
  let books =
    auth
      (eur "23.00")
      ~id:"a2"
      ~merchant:(merchant ~name:"BOOKSHOP NYC" (usd "25.00"))
      ~at:20
  in
  let hotel =
    auth
      (eur "200.00")
      ~id:"a3"
      ~merchant:(merchant ~name:"HOTEL LISBOA" ~mcc:Mcc.lodging (eur "200.00"))
      ~at:30
  in
  let declined =
    snd
      (authorize
         (input ~available:(eur "1.00") (request ~auth_id:"a4" ~at:(at 40) (eur "99.00"))))
  in
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
  let c1 =
    clearing
      ~id:"c1"
      ~auth_id:(Some "a1")
      ~merchant:coffee.merchant
      ~at:(at 1440)
      (eur "3.80")
  in
  let c2 =
    clearing
      ~id:"c2"
      ~auth_id:(Some "a2")
      ~merchant:books.merchant
      ~rate
      ~fx_fee
      ~at:(at 1500)
      (usd "25.00")
  in
  let r1 =
    clearing
      ~id:"r1"
      ~auth_id:(Some "a1")
      ~direction:Credit
      ~merchant:coffee.merchant
      ~at:(at 3000)
      (eur "3.80")
  in
  let settle_auth a c =
    match Settlement.settle c ~auth:(Some a) |> Or_error.ok_exn with
    | Against_auth { auth; _ } -> auth
    | _ -> a
  in
  let coffee = settle_auth coffee c1 in
  let books = settle_auth books c2 in
  let statement =
    Statement.create
      ~card
      ~authorizations:[ coffee; books; hotel; declined ]
      ~clearings:[ c1; c2; r1 ]
      ~from:start
      ~until:(Time_ns.add start (days 30))
      ~include_declined:true
      ()
    |> Or_error.ok_exn
  in
  print_string (Statement.to_string_hum statement);
  [%expect
    {|
    Card **** 4242 10/29, 2026-10-13 to 2026-11-12
    2026-10-13  pending      HOTEL LISBOA                  -200.00 EUR
    2026-10-13  declined 51  COFFEE BAR BERLIN              -99.00 EUR
    2026-10-14  purchase     COFFEE BAR BERLIN               -3.80 EUR
    2026-10-14  purchase     BOOKSHOP NYC                   -23.00 EUR  (25.00 USD)
    2026-10-14  fx fee       BOOKSHOP NYC                    -0.23 EUR
    2026-10-15  refund       COFFEE BAR BERLIN                3.80 EUR
    spent                                                     27.03 EUR
    refunded                                                   3.80 EUR
    pending                                                  200.00 EUR
    |}]
;;
