open! Core
module Amount = Money.Amount

module Kind = struct
  type t =
    | Pending
    | Purchase
    | Refund
    | Fx_fee
    | Declined of Response_code.t
  [@@deriving sexp_of, compare, equal]

  let to_string = function
    | Pending -> "pending"
    | Purchase -> "purchase"
    | Refund -> "refund"
    | Fx_fee -> "fx fee"
    | Declined code -> "declined " ^ Response_code.to_string code
  ;;
end

module Line = struct
  type t =
    { at : Payments_core.Utc_time.t
    ; kind : Kind.t
    ; description : string
    ; amount : Amount.t
    ; original : Amount.t option
    ; auth_id : Ids.Auth_id.t option
    ; clearing_id : Ids.Clearing_id.t option
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { card_id : Ids.Card_id.t
  ; masked_pan : Masked_pan.t option
  ; from : Payments_core.Utc_time.t
  ; until : Payments_core.Utc_time.t
  ; lines : Line.t list
  ; spent : Amount.t
  ; refunded : Amount.t
  ; pending : Amount.t
  }
[@@deriving sexp_of]

let create
  ~(card : Card.t)
  ~(authorizations : Authorization.t list)
  ~(clearings : Settlement.Clearing.t list)
  ~from
  ~until
  ?(include_declined = false)
  ()
  =
  let open Or_error.Let_syntax in
  let in_period at = Time_ns.( >= ) at from && Time_ns.( < ) at until in
  let auths =
    List.filter authorizations ~f:(fun a ->
      Ids.Card_id.equal a.card_id card.id && in_period a.created_at)
  in
  let auth_lines =
    List.filter_map auths ~f:(fun a ->
      let line kind amount =
        { Line.at = a.created_at
        ; kind
        ; description = a.merchant.name
        ; amount
        ; original =
            Option.some_if
              (not (Money.Currency.equal (Amount.currency a.merchant.amount) (Amount.currency a.requested)))
              a.merchant.amount
        ; auth_id = Some a.id
        ; clearing_id = None
        }
      in
      match a.status with
      | Declined _ when include_declined ->
        Some
          (line
             (Declined (Option.value a.response_code ~default:Response_code.Do_not_honor))
             (Amount.neg a.requested))
      | Authorized | Partially_cleared -> Some (line Pending (Amount.neg (Authorization.open_hold a)))
      | Pending | Declined _ | Cleared | Reversed | Expired -> None)
  in
  let clearing_lines =
    List.filter clearings ~f:(fun c -> Ids.Card_id.equal c.card_id card.id && in_period c.at)
    |> List.concat_map ~f:(fun (c : Settlement.Clearing.t) ->
      let line kind amount =
        { Line.at = c.at
        ; kind
        ; description = c.merchant.name
        ; amount
        ; original = Option.map c.fx ~f:(fun _ -> c.merchant.amount)
        ; auth_id = c.auth_id
        ; clearing_id = Some c.id
        }
      in
      match c.direction with
      | Credit -> [ line Refund c.billing ]
      | Debit ->
        line Purchase (Amount.neg c.billing)
        :: (match c.fx with
            | Some { fee; _ } when Amount.is_positive fee ->
              [ { (line Fx_fee (Amount.neg fee)) with original = None } ]
            | Some _ | None -> []))
  in
  let lines =
    List.sort (auth_lines @ clearing_lines) ~compare:(fun (a : Line.t) b ->
      match Time_ns.compare a.at b.at with
      | 0 -> Line.compare a b
      | c -> c)
  in
  let total kinds =
    List.filter lines ~f:(fun (l : Line.t) -> List.mem kinds l.kind ~equal:Kind.equal)
    |> List.map ~f:(fun l -> Amount.abs l.amount)
    |> Amount.sum card.currency
  in
  let%bind spent = total [ Purchase; Fx_fee ] in
  let%bind refunded = total [ Refund ] in
  let%map pending = total [ Pending ] in
  { card_id = card.id
  ; masked_pan = card.masked_pan
  ; from
  ; until
  ; lines
  ; spent
  ; refunded
  ; pending
  }
;;

let to_string_hum t =
  let buf = Buffer.create 1024 in
  let date at = Time_ns.to_date at ~zone:Time_float.Zone.utc |> Date.to_string in
  bprintf
    buf
    "Card %s, %s to %s\n"
    (Option.value_map t.masked_pan ~default:(Ids.Card_id.to_string t.card_id) ~f:Masked_pan.to_string)
    (date t.from)
    (date t.until);
  List.iter t.lines ~f:(fun l ->
    bprintf
      buf
      "%s  %-12s %-24s %16s%s\n"
      (date l.at)
      (Kind.to_string l.kind)
      (String.prefix l.description 24)
      (Amount.to_string l.amount)
      (Option.value_map l.original ~default:"" ~f:(fun o -> "  (" ^ Amount.to_string o ^ ")")));
  bprintf buf "%-50s %16s\n" "spent" (Amount.to_string t.spent);
  bprintf buf "%-50s %16s\n" "refunded" (Amount.to_string t.refunded);
  bprintf buf "%-50s %16s\n" "pending" (Amount.to_string t.pending);
  Buffer.contents buf
;;
