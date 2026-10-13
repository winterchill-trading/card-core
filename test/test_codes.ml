open! Core
open Card_core

let%expect_test "response codes, categories and retry" =
  List.iter Response_code.all_known ~f:(fun code ->
    printf
      "%s %-20s retry=%b\n"
      (Response_code.to_string code)
      (match Response_code.category code with
       | None -> "-"
       | Some c -> Sexp.to_string [%sexp (c : Response_code.Category.t)])
      (Response_code.retry_allowed code));
  [%expect
    {|
    00 -                    retry=true
    05 Generic              retry=true
    14 Never_approve        retry=false
    46 Never_approve        retry=false
    51 Cannot_approve_now   retry=true
    54 Data_quality         retry=true
    55 Data_quality         retry=true
    57 Never_approve        retry=false
    61 Cannot_approve_now   retry=true
    62 Never_approve        retry=false
    65 Cannot_approve_now   retry=true
    83 Never_approve        retry=false
    91 Cannot_approve_now   retry=true
    1A Data_quality         retry=true
    |}]
;;

let%expect_test "parsing" =
  let show s =
    print_s [%sexp (Response_code.of_string_or_error s : Response_code.t Or_error.t)]
  in
  show "51";
  show "1A";
  show "N7";
  show "5";
  show "a1";
  [%expect
    {|
    (Ok 51)
    (Ok 1A)
    (Ok N7)
    (Error ("Response_code: expected two characters [0-9A-Z]" 5))
    (Error ("Response_code: expected two characters [0-9A-Z]" a1))
    |}]
;;

let%test_unit "response code text round trip" =
  Quickcheck.test
    (Quickcheck.Generator.of_list Response_code.all_known)
    ~f:(fun code ->
      [%test_result: Response_code.t]
        (Response_code.of_string_or_error (Response_code.to_string code) |> Or_error.ok_exn)
        ~expect:code)
;;

let%expect_test "merchant category codes" =
  print_s [%sexp (Mcc.of_string_or_error "0742" : Mcc.t Or_error.t)];
  print_s [%sexp (Mcc.of_int 10000 : Mcc.t Or_error.t)];
  print_s
    [%sexp
      (List.map [ Mcc.lodging; Mcc.vehicle_rental; Mcc.cruise; Mcc.fuel; Mcc.atm ] ~f:(fun m ->
         m, Mcc.has_long_hold m, Mcc.is_atm m)
       : (Mcc.t * bool * bool) list)];
  [%expect
    {|
    (Ok 0742)
    (Error ("Mcc: expected 0000 to 9999" (i 10000)))
    ((7011 true false) (7512 true false) (4411 true false) (5542 false false)
     (6011 false true))
    |}]
;;

let%expect_test "countries" =
  print_s [%sexp (Country.of_string_or_error "DE" : Country.t Or_error.t)];
  print_s [%sexp (Country.of_string_or_error "deu" : Country.t Or_error.t)];
  [%expect
    {|
    (Ok DE)
    (Error ("Country: expected two upper-case letters" deu))
    |}]
;;
