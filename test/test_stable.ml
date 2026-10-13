open! Core
open Card_core
open Helpers

(* A changed digest means old bytes no longer decode: add a Stable.V2 instead of editing
   V1, and keep V1 readable. *)
let%expect_test "stable bin_prot digests" =
  List.iter
    [ "Response_code", [%bin_digest: Response_code.Stable.V1.t]
    ; "Masked_pan", [%bin_digest: Masked_pan.Stable.V1.t]
    ; "Limits", [%bin_digest: Limits.Stable.V1.t]
    ; "Card", [%bin_digest: Card.Stable.V1.t]
    ; "Three_ds", [%bin_digest: Three_ds.Stable.V1.t]
    ; "Merchant", [%bin_digest: Merchant.Stable.V1.t]
    ; "Decline_reason", [%bin_digest: Decline_reason.Stable.V1.t]
    ; "Authorization", [%bin_digest: Authorization.Stable.V1.t]
    ; "Clearing", [%bin_digest: Settlement.Clearing.Stable.V1.t]
    ; "Dispute", [%bin_digest: Dispute.Stable.V1.t]
    ]
    ~f:(fun (name, digest) -> printf "%-16s %s\n" name digest);
  [%expect
    {|
    Response_code    d9a8da25d5656b016fb4dbdc2e4197fb
    Masked_pan       ad817a0bc36a121fd4e9b42633523ff4
    Limits           bfac8330a0950dc904abb5086e52c41b
    Card             dde990b23f3cbf94c6c86cc8921cdf01
    Three_ds         15a271a65d246fd74a3afd2ca3eec3ec
    Merchant         6ad503fb49a915dc208ff1cb1fd905c9
    Decline_reason   16b5a963b14cd1f055b3525dce0dc8df
    Authorization    f95053b248c8a3d1ac61e269f20a4f5d
    Clearing         bc5bfc3116d0537a52e81986ee568c08
    Dispute          4d99f8f2f6d804caffd4478feb781228
    |}]
;;

let round_trip
  (type a)
  (module M : Binable.S with type t = a)
  ~sexp_of
  ~of_sexp
  ~equal
  (v : a)
  =
  let bytes = Binable.to_string (module M) v in
  let back = Binable.of_string (module M) bytes in
  assert (equal back v);
  assert (equal (of_sexp (sexp_of v)) v)
;;

let%expect_test "records round trip through bin_prot and sexp" =
  let _, auth =
    authorize
      (input
         (request
            ~three_ds:{ trans_status = Y; eci = Some 5; cavv_present = true }
            (eur "12.50")))
  in
  round_trip
    (module Authorization.Stable.V1)
    ~sexp_of:Authorization.Stable.V1.sexp_of_t
    ~of_sexp:Authorization.Stable.V1.t_of_sexp
    ~equal:Authorization.Stable.V1.equal
    auth;
  round_trip
    (module Card.Stable.V1)
    ~sexp_of:Card.Stable.V1.sexp_of_t
    ~of_sexp:Card.Stable.V1.t_of_sexp
    ~equal:Card.Stable.V1.equal
    (active_card ());
  let c = clearing (eur "12.50") in
  round_trip
    (module Settlement.Clearing.Stable.V1)
    ~sexp_of:Settlement.Clearing.Stable.V1.sexp_of_t
    ~of_sexp:Settlement.Clearing.Stable.V1.t_of_sexp
    ~equal:Settlement.Clearing.Stable.V1.equal
    c;
  print_s [%sexp (auth : Authorization.Stable.V1.t)];
  [%expect
    {|
    ((id auth-1) (card_id card-1) (network_txn_id ntx-1) (kind Auth)
     (requested "12.50 EUR") (approved "12.50 EUR")
     (merchant
      ((name "COFFEE BAR BERLIN") (mcc 5411) (country DE) (amount "12.50 EUR")))
     (entry_mode Chip) (initiator Cardholder) (estimated false)
     (three_ds (((trans_status Y) (eci (5)) (cavv_present true))))
     (sca ((Challenge 3ds))) (response_code (00)) (decided_by Us)
     (status Authorized) (increments ()) (captured "0.00 EUR")
     (cleared "0.00 EUR") (released "0.00 EUR") (refunded "0.00 EUR")
     (created_at "2026-10-13 09:00:00Z") (expires_at "2026-10-20 09:00:00Z")
     (updated_at "2026-10-13 09:00:00Z"))
    |}]
;;

let%expect_test "decoding validates: a full card number cannot be read back as last four" =
  let bytes = Binable.to_string (module String) "4111111111111111" in
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         Binable.of_string (module Masked_pan.Last4.Stable.V1) bytes)
       : Masked_pan.Last4.t Or_error.t)];
  [%expect {| (Error ("Masked_pan.Last4: expected exactly four digits" (length 16))) |}]
;;
