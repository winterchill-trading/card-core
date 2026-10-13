open! Core
open Card_core
open Helpers
module Amount = Money.Amount

(* Random card activity, applied to both the authorization records and a ledger. After
   every step the two must agree: each authorization's open hold equals its ledger hold,
   the account's held funds equal the sum of open holds, and money is conserved. *)

module Op = struct
  type t =
    | Approve of int (** cents *)
    | Increment of int * int (** auth index, cents *)
    | Reverse of int * int (** auth index, percent of the open hold *)
    | Clear of int * int * bool (** auth index, percent of the approved amount, final *)
    | Clear_without_auth of int
    | Refund of int * int
    | Wait_days of int
  [@@deriving sexp_of, quickcheck]

  let quickcheck_generator =
    let open Quickcheck.Generator in
    let cents = Int.gen_incl 1 5_000 in
    let idx = Int.gen_incl 0 5 in
    weighted_union
      [ 4., map cents ~f:(fun c -> Approve c)
      ; 1., map (both idx cents) ~f:(fun (i, c) -> Increment (i, c))
      ; 1., map (both idx (Int.gen_incl 1 100)) ~f:(fun (i, p) -> Reverse (i, p))
      ; ( 3.
        , map (tuple3 idx (Int.gen_incl 1 150) Bool.quickcheck_generator) ~f:(fun (i, p, f) ->
            Clear (i, p, f)) )
      ; 1., map cents ~f:(fun c -> Clear_without_auth c)
      ; 1., map (both idx cents) ~f:(fun (i, c) -> Refund (i, c))
      ; 1., map (Int.gen_incl 1 10) ~f:(fun d -> Wait_days d)
      ]
  ;;
end

type state =
  { b : Ledger.Balances.t
  ; auths : Authorization.t list (** newest last *)
  ; now : Time_ns.t
  ; n : int (** event counter for ids and keys *)
  ; cleared : Amount.t
  ; refunded : Amount.t
  }

let cents c = Amount.of_minor_units_int Money.Currency.eur c
let deposit = eur "300.00"
let card = active_card ~limits:(Limits.unlimited Money.Currency.eur) ()

let replace auths (a : Authorization.t) =
  List.map auths ~f:(fun (x : Authorization.t) -> if Ids.Auth_id.equal x.id a.id then a else x)
;;

let nth auths i = if List.is_empty auths then None else List.nth auths (i % List.length auths)

let percent amount p = Amount.mul_ratio amount (Q.of_ints p 100) ~rounding:Down

let step st (op : Op.t) =
  let n = st.n + 1 in
  let now = Time_ns.add st.now (Time_ns.Span.of_int_sec 60) in
  let st = { st with n; now } in
  let event = sprintf "evt-%d" n in
  let settle (auth : Authorization.t option) c =
    let outcome = Settlement.settle c ~auth |> Or_error.ok_exn in
    let b = book st.b (Booking.settled chart st.b card c outcome ~event_id:event) in
    let auths =
      match outcome with
      | Against_auth { auth; _ } | Refund { auth = Some auth } -> replace st.auths auth
      | Refund { auth = None } | Without_auth -> st.auths
    in
    b, auths
  in
  match op with
  | Wait_days d ->
    let now = Time_ns.add now (days d) in
    (* Expire every lapsed hold, as a daily job would. *)
    List.fold st.auths ~init:{ st with now } ~f:(fun st a ->
      if Authorization.is_due_to_expire a ~now
      then (
        let a, _ = Authorization.expire a ~now |> Or_error.ok_exn in
        let b = book st.b (Booking.expired st.b a ~event_id:(event ^ Ids.Auth_id.to_string a.id) ~at:now) in
        { st with b; auths = replace st.auths a })
      else st)
  | Approve c ->
    let available = Ledger.Balances.available st.b account Money.Currency.eur in
    let r =
      request
        ~auth_id:(sprintf "a%d" n)
        ~network_txn_id:(sprintf "n%d" n)
        ~partial_supported:true
        ~at:now
        (cents c)
    in
    let decision, auth = authorize (input ~card ~available r) in
    (match decision with
     | Decline _ -> st
     | Approve _ ->
       let b = book st.b (Booking.authorized st.b card auth ~event_id:event ~at:now) in
       { st with b; auths = st.auths @ [ auth ] })
  | Increment (i, c) ->
    (match nth st.auths i with
     | Some a when Authorization.Status.is_open a.status ->
       let available = Ledger.Balances.available st.b account Money.Currency.eur in
       if Amount.( > ) (cents c) available
       then st
       else (
         let r =
           request
             ~auth_id:(sprintf "i%d" n)
             ~network_txn_id:(Ids.Network_txn_id.to_string a.network_txn_id)
             ~kind:Incremental
             ~at:now
             (cents c)
         in
         let a = Authorization.increment a r ~approved:(cents c) |> Or_error.ok_exn in
         let b = book st.b (Booking.incremented st.b a ~amount:(cents c) ~event_id:event ~at:now) in
         { st with b; auths = replace st.auths a })
     | Some _ | None -> st)
  | Reverse (i, p) ->
    (match nth st.auths i with
     | Some a when Amount.is_positive (Authorization.open_hold a) && not (Authorization.is_due_to_expire a ~now) ->
       let amount = percent (Authorization.open_hold a) p in
       if Amount.is_zero amount
       then st
       else (
         let a, released = Authorization.reverse a ~amount ~at:now () |> Or_error.ok_exn in
         let b = book st.b (Booking.reversed st.b a ~amount:released ~event_id:event ~at:now) in
         { st with b; auths = replace st.auths a })
     | Some _ | None -> st)
  | Clear (i, p, final) ->
    (match nth st.auths i with
     | Some a ->
       let amount = percent a.approved p in
       if Amount.is_zero amount
       then st
       else (
         let c =
           clearing
             ~id:(sprintf "c%d" n)
             ~auth_id:(Some (Ids.Auth_id.to_string a.id))
             ~final
             ~at:now
             amount
         in
         let b, auths = settle (Some a) c in
         { st with b; auths; cleared = Amount.add_exn st.cleared amount })
     | None -> st)
  | Clear_without_auth c ->
    let cl = clearing ~id:(sprintf "c%d" n) ~auth_id:None ~at:now (cents c) in
    let b, auths = settle None cl in
    { st with b; auths; cleared = Amount.add_exn st.cleared (cents c) }
  | Refund (i, c) ->
    let auth = nth st.auths i in
    let cl =
      clearing
        ~id:(sprintf "r%d" n)
        ~auth_id:(Option.map auth ~f:(fun a -> Ids.Auth_id.to_string a.id))
        ~direction:Credit
        ~at:now
        (cents c)
    in
    let b, auths = settle auth cl in
    { st with b; auths; refunded = Amount.add_exn st.refunded (cents c) }
;;

let check st =
  let cur = Money.Currency.eur in
  List.iter st.auths ~f:(fun a ->
    let hold_id = Booking.hold_id a.id |> Or_error.ok_exn in
    let ledger_remaining =
      match Ledger.Balances.find_hold st.b hold_id with
      | Some h -> h.remaining
      | None -> Amount.zero cur
    in
    [%test_result: Amount.t]
      ledger_remaining
      ~expect:(Authorization.open_hold a)
      ~message:(Ids.Auth_id.to_string a.id));
  let open_holds = Amount.sum cur (List.map st.auths ~f:Authorization.open_hold) |> Or_error.ok_exn in
  [%test_result: Amount.t] (Ledger.Balances.held st.b account cur) ~expect:open_holds;
  (* Money is conserved: what the cardholder has, plus what they owe us, is the deposit
     less every clearing plus every refund. *)
  let posted = Ledger.Balances.posted st.b account cur in
  let owed = Ledger.Balances.balance st.b (chart.card_receivable cur) cur in
  [%test_result: Amount.t]
    (Amount.sub_exn posted owed)
    ~expect:(Amount.add_exn (Amount.sub_exn deposit st.cleared) st.refunded);
  [%test_result: Amount.t]
    (Amount.neg (Ledger.Balances.balance st.b (chart.card_settlement cur) cur))
    ~expect:(Amount.sub_exn st.cleared st.refunded);
  assert (not (Amount.is_negative (Ledger.Balances.available st.b account cur)));
  Ledger.Trial_balance.of_balances st.b |> Ledger.Trial_balance.check |> Or_error.ok_exn
;;

let%test_unit "holds and captures reconcile with the ledger" =
  Quickcheck.test
    ~trials:300
    (List.gen_with_length 30 Op.quickcheck_generator)
    ~sexp_of:[%sexp_of: Op.t list]
    ~f:(fun ops ->
      let init =
        { b = ledger ~deposit ()
        ; auths = []
        ; now = start
        ; n = 0
        ; cleared = Amount.zero Money.Currency.eur
        ; refunded = Amount.zero Money.Currency.eur
        }
      in
      ignore
        (List.fold ops ~init ~f:(fun st op ->
           let st = step st op in
           check st;
           st)
         : state))
;;

(* Approved authorizations never add up to more than a cap, whatever the order, size,
   merchant or day of the requests. *)
let%test_unit "limits are never exceeded by approved authorizations" =
  let limits = limits () in
  let card = active_card ~limits () in
  Quickcheck.test
    ~trials:200
    (List.gen_with_length
       60
       (Quickcheck.Generator.tuple4
          (Int.gen_incl 1 60_000)
          Bool.quickcheck_generator
          Bool.quickcheck_generator
          (Int.gen_incl 0 120)))
    ~sexp_of:[%sexp_of: (int * bool * bool * int) list]
    ~f:(fun requests ->
      let _, auths =
        List.foldi requests ~init:(start, []) ~f:(fun i (now, auths) (c, atm, partial, minutes) ->
          let now = Time_ns.add now (Time_ns.Span.of_int_min (minutes * 10)) in
          let mcc = if atm then Mcc.atm else grocery in
          let r =
            request
              ~auth_id:(sprintf "a%d" i)
              ~network_txn_id:(sprintf "n%d" i)
              ~merchant:(merchant ~mcc (cents c))
              ~partial_supported:partial
              ~at:now
              (cents c)
          in
          let usage = Authorization.usage auths ~currency:Money.Currency.eur ~now |> Or_error.ok_exn in
          let _, auth = authorize (input ~card ~usage ~available:(eur "100000.00") r) in
          now, auth :: auths)
      in
      let approved = List.filter auths ~f:(fun a -> Authorization.Status.is_open a.status) in
      let by key =
        List.map approved ~f:(fun (a : Authorization.t) -> key a, a)
        |> List.Assoc.sort_and_group ~compare:[%compare: string]
      in
      let date (a : Authorization.t) = Date.to_string (Time_ns.to_date a.created_at ~zone:Time_float.Zone.utc) in
      let month (a : Authorization.t) = String.prefix (date a) 7 in
      let sum xs = Amount.sum Money.Currency.eur (List.map xs ~f:Authorization.spend) |> Or_error.ok_exn in
      let cap = Option.value_exn in
      List.iter approved ~f:(fun a -> assert (Amount.( <= ) (Authorization.spend a) (cap limits.per_transaction)));
      List.iter (by date) ~f:(fun (_, xs) ->
        assert (Amount.( <= ) (sum xs) (cap limits.daily));
        let atm = List.filter xs ~f:(fun a -> Mcc.is_atm a.merchant.mcc) in
        assert (Amount.( <= ) (sum atm) (cap limits.atm_daily));
        assert (List.length atm <= cap limits.atm_daily_count));
      List.iter (by month) ~f:(fun (_, xs) -> assert (Amount.( <= ) (sum xs) (cap limits.monthly))))
;;
