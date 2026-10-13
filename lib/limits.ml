open! Core

module Amount = Money.Amount

module Usage = struct
  type t =
    { daily : Amount.t
    ; monthly : Amount.t
    ; atm_daily : Amount.t
    ; atm_daily_count : int
    }
  [@@deriving sexp, compare, equal]

  let zero currency =
    { daily = Amount.zero currency
    ; monthly = Amount.zero currency
    ; atm_daily = Amount.zero currency
    ; atm_daily_count = 0
    }
  ;;

  let add t amount ~atm =
    let open Or_error.Let_syntax in
    let%bind daily = Amount.add t.daily amount in
    let%bind monthly = Amount.add t.monthly amount in
    let%map atm_daily = if atm then Amount.add t.atm_daily amount else Ok t.atm_daily in
    { daily
    ; monthly
    ; atm_daily
    ; atm_daily_count = (t.atm_daily_count + if atm then 1 else 0)
    }
  ;;
end

module Violation = struct
  type t =
    | Per_transaction
    | Daily
    | Monthly
    | Atm_amount
    | Atm_count
    | Atm_disabled
    | Online_disabled
    | Contactless_disabled
    | Mcc_blocked of Mcc.t
    | Country_blocked of Country.t
  [@@deriving sexp, compare, equal]

  let response_code : t -> Response_code.t = function
    | Per_transaction | Daily | Monthly | Atm_amount -> Exceeds_amount_limit
    | Atm_count -> Exceeds_frequency_limit
    | Atm_disabled
    | Online_disabled
    | Contactless_disabled
    | Mcc_blocked _
    | Country_blocked _ -> Not_permitted
  ;;
end

module Stable = struct
  module V1 = struct
    module Wire = struct
      type t =
        { currency : Money.Currency.Stable.V1.t
        ; per_transaction : Amount.Stable.V1.t option
        ; daily : Amount.Stable.V1.t option
        ; monthly : Amount.Stable.V1.t option
        ; atm_daily : Amount.Stable.V1.t option
        ; atm_daily_count : int option
        ; online : bool
        ; contactless : bool
        ; atm : bool
        ; blocked_mccs : Mcc.Stable.V1.t list
        ; blocked_countries : Country.Stable.V1.t list
        }
      [@@deriving bin_io, sexp, compare, equal]
    end

    type t =
      { currency : Money.Currency.t
      ; per_transaction : Amount.t option
      ; daily : Amount.t option
      ; monthly : Amount.t option
      ; atm_daily : Amount.t option
      ; atm_daily_count : int option
      ; online : bool
      ; contactless : bool
      ; atm : bool
      ; blocked_mccs : Mcc.Set.t
      ; blocked_countries : Country.Set.t
      }
    [@@deriving sexp_of, compare, equal]

    let validate (t : t) =
      let cap name = function
        | None -> Ok ()
        | Some amount ->
          if not (Money.Currency.equal (Amount.currency amount) t.currency)
          then
            Or_error.error_s
              [%message
                "Limits: cap in another currency"
                  name
                  (amount : Amount.t)
                  ~currency:(t.currency : Money.Currency.t)]
          else if not (Amount.is_positive amount)
          then Or_error.error_s [%message "Limits: cap must be positive" name (amount : Amount.t)]
          else Ok ()
      in
      let open Or_error.Let_syntax in
      let%bind () = cap "per_transaction" t.per_transaction in
      let%bind () = cap "daily" t.daily in
      let%bind () = cap "monthly" t.monthly in
      let%bind () = cap "atm_daily" t.atm_daily in
      match t.atm_daily_count with
      | Some n when n <= 0 ->
        Or_error.error_s [%message "Limits: ATM count must be positive" (n : int)]
      | _ -> Ok t
    ;;

    let to_wire (t : t) : Wire.t =
      { currency = t.currency
      ; per_transaction = t.per_transaction
      ; daily = t.daily
      ; monthly = t.monthly
      ; atm_daily = t.atm_daily
      ; atm_daily_count = t.atm_daily_count
      ; online = t.online
      ; contactless = t.contactless
      ; atm = t.atm
      ; blocked_mccs = Set.to_list t.blocked_mccs
      ; blocked_countries = Set.to_list t.blocked_countries
      }
    ;;

    let of_wire (w : Wire.t) : t Or_error.t =
      validate
        { currency = w.currency
        ; per_transaction = w.per_transaction
        ; daily = w.daily
        ; monthly = w.monthly
        ; atm_daily = w.atm_daily
        ; atm_daily_count = w.atm_daily_count
        ; online = w.online
        ; contactless = w.contactless
        ; atm = w.atm
        ; blocked_mccs = Mcc.Set.of_list w.blocked_mccs
        ; blocked_countries = Country.Set.of_list w.blocked_countries
        }
    ;;

    let t_of_sexp sexp = Wire.t_of_sexp sexp |> of_wire |> Or_error.ok_exn
    let sexp_of_t t = Wire.sexp_of_t (to_wire t)

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Wire)
        (struct
          type nonrec t = t

          let to_binable = to_wire
          let of_binable w = of_wire w |> Or_error.ok_exn
        end)
  end
end

include Stable.V1

let create
  ?per_transaction
  ?daily
  ?monthly
  ?atm_daily
  ?atm_daily_count
  ?(online = true)
  ?(contactless = true)
  ?(atm = true)
  ?(blocked_mccs = [])
  ?(blocked_countries = [])
  currency
  =
  validate
    { currency
    ; per_transaction
    ; daily
    ; monthly
    ; atm_daily
    ; atm_daily_count
    ; online
    ; contactless
    ; atm
    ; blocked_mccs = Mcc.Set.of_list blocked_mccs
    ; blocked_countries = Country.Set.of_list blocked_countries
    }
;;

let unlimited currency = create currency |> Or_error.ok_exn
let with_online t online = { t with online }
let with_contactless t contactless = { t with contactless }
let with_atm t atm = { t with atm }

let check_currency t amount =
  if Money.Currency.equal (Amount.currency amount) t.currency
  then Ok ()
  else
    Or_error.error_s
      [%message
        "Limits: amount not in the billing currency"
          (amount : Amount.t)
          ~billing:(t.currency : Money.Currency.t)]
;;

(* [Some excess] when [used + amount] is over [cap]. *)
let over ~cap ~used amount =
  match cap with
  | None -> false
  | Some cap -> Amount.( > ) (Amount.add_exn used amount) cap
;;

let check t ~(usage : Usage.t) ~amount ~mcc ~country ~entry_mode =
  let open Or_error.Let_syntax in
  let%map () = check_currency t amount in
  let atm = Mcc.is_atm mcc in
  let zero = Amount.zero t.currency in
  let rules : (bool * Violation.t) list =
    [ atm && not t.atm, Atm_disabled
    ; Entry_mode.is_card_absent entry_mode && not t.online, Online_disabled
    ; Entry_mode.equal entry_mode Contactless && not t.contactless, Contactless_disabled
    ; Set.mem t.blocked_mccs mcc, Mcc_blocked mcc
    ; Set.mem t.blocked_countries country, Country_blocked country
    ; over ~cap:t.per_transaction ~used:zero amount, Per_transaction
    ; over ~cap:t.daily ~used:usage.daily amount, Daily
    ; over ~cap:t.monthly ~used:usage.monthly amount, Monthly
    ; atm && over ~cap:t.atm_daily ~used:usage.atm_daily amount, Atm_amount
    ; ( atm
        && Option.value_map t.atm_daily_count ~default:false ~f:(fun n ->
          usage.atm_daily_count >= n)
      , Atm_count )
    ]
  in
  match List.find rules ~f:fst with
  | None -> Ok ()
  | Some (_, violation) -> Error violation
;;

let headroom t ~(usage : Usage.t) ~mcc =
  let open Or_error.Let_syntax in
  let%bind () = check_currency t usage.daily in
  let left ~cap ~used =
    Option.map cap ~f:(fun cap ->
      let rest = Amount.sub_exn cap used in
      if Amount.is_negative rest then Amount.zero t.currency else rest)
  in
  let zero = Amount.zero t.currency in
  let candidates =
    [ left ~cap:t.per_transaction ~used:zero
    ; left ~cap:t.daily ~used:usage.daily
    ; left ~cap:t.monthly ~used:usage.monthly
    ; (if Mcc.is_atm mcc then left ~cap:t.atm_daily ~used:usage.atm_daily else None)
    ]
    |> List.filter_opt
  in
  return (List.min_elt candidates ~compare:Amount.compare)
;;
