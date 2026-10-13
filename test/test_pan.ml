open! Core
open Card_core

let%expect_test "last four digits only" =
  print_s
    [%sexp (Masked_pan.Last4.of_string_or_error "4242" : Masked_pan.Last4.t Or_error.t)];
  (* A full test card number is refused without being echoed back. *)
  print_s
    [%sexp
      (Masked_pan.Last4.of_string_or_error "4111111111111111"
       : Masked_pan.Last4.t Or_error.t)];
  [%expect
    {|
    (Ok 4242)
    (Error ("Masked_pan.Last4: expected exactly four digits" (length 16)))
    |}]
;;

let%expect_test "expiry runs through the end of the month (UTC)" =
  let expiry = Masked_pan.Expiry.create ~month:12 ~year:2029 |> Or_error.ok_exn in
  let at s = Time_ns.of_string_with_utc_offset s in
  print_endline (Masked_pan.Expiry.to_string expiry);
  print_endline (Time_ns.to_string_utc (Masked_pan.Expiry.end_ expiry));
  printf
    "%b %b\n"
    (Masked_pan.Expiry.is_expired expiry ~now:(at "2029-12-31 23:59:59Z"))
    (Masked_pan.Expiry.is_expired expiry ~now:(at "2030-01-01 00:00:00Z"));
  print_s
    [%sexp
      (Masked_pan.Expiry.create ~month:13 ~year:2029 : Masked_pan.Expiry.t Or_error.t)];
  let masked = { Masked_pan.last4 = Masked_pan.Last4.of_string "4242"; expiry } in
  print_endline (Masked_pan.to_string masked);
  print_s [%sexp (masked : Masked_pan.t)];
  [%expect
    {|
    12/29
    2030-01-01 00:00:00.000000000Z
    false true
    (Error ("Expiry: month must be 1 to 12" (month 13)))
    **** 4242 12/29
    ((last4 4242) (expiry ((month 12) (year 2029))))
    |}]
;;

let%expect_test "display tokens never print" =
  let token =
    Pan_display_token.of_provider ~token:"tok_live_abcdef" ~expires_at:Time_ns.epoch
    |> Or_error.ok_exn
  in
  print_s [%sexp (token : Pan_display_token.t)];
  print_s [%sexp { token : Pan_display_token.t }];
  [%expect {|
    <pan-display-token>
    ((token <pan-display-token>))
    |}]
;;

let%expect_test "card numbers in free text" =
  List.iter
    [ "COFFEE BAR BERLIN"
    ; "card 4111 1111 1111 1111 declined"
    ; "ref 4111-1111-1111-1111"
    ; "4111111111111112"
    ; "order 12345678901234567890123"
    ; "5555555555554444"
    ]
    ~f:(fun s -> printf "%-36s %b %s\n" s (Pan_guard.contains_pan s) (Pan_guard.scrub s));
  print_s
    [%sexp
      (Pan_guard.check ~field:"merchant_name" "x 4242424242424242" : string Or_error.t)];
  [%expect
    {|
    COFFEE BAR BERLIN                    false COFFEE BAR BERLIN
    card 4111 1111 1111 1111 declined    true card ************1111 declined
    ref 4111-1111-1111-1111              true ref ************1111
    4111111111111112                     false 4111111111111112
    order 12345678901234567890123        false order 12345678901234567890123
    5555555555554444                     true ************4444
    (Error ("text looks like it contains a card number" merchant_name))
    |}]
;;

let%test_unit "luhn: changing one digit breaks the check" =
  Quickcheck.test
    Quickcheck.Generator.(
      both (String.gen_with_length 15 Char.gen_digit) (Int.gen_incl 0 14))
    ~f:(fun (body, i) ->
      (* Append the check digit, then change one body digit. *)
      let check =
        List.find_exn (List.range 0 10) ~f:(fun d ->
          Pan_guard.luhn_valid (body ^ Int.to_string d))
      in
      let pan = body ^ Int.to_string check in
      assert (Pan_guard.luhn_valid pan);
      let changed = Bytes.of_string pan in
      let d = Char.get_digit_exn pan.[i] in
      Bytes.set changed i (Char.of_int_exn (Char.to_int '0' + ((d + 1) % 10)));
      assert (not (Pan_guard.luhn_valid (Bytes.to_string changed))))
;;
