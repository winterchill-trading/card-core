(** What one card did in a period, for the cardholder: purchases (clearings), refunds,
    FX fees, holds still pending, and optionally declined attempts.

    Built from the authorizations and clearings the caller has stored; the funding
    account's ledger statement is the authoritative balance view, this is the card view
    (merchant names, original currencies, pending holds). Lines are sorted by time, then
    by id. *)

open! Core

module Kind : sig
  type t =
    | Pending (** an open hold: may still change or disappear *)
    | Purchase
    | Refund
    | Fx_fee
    | Declined of Response_code.t
  [@@deriving sexp_of, compare, equal]
end

module Line : sig
  type t =
    { at : Payments_core.Utc_time.t
    ; kind : Kind.t
    ; description : string (** the merchant name *)
    ; amount : Money.Amount.t (** billing currency; spend negative, refunds positive *)
    ; original : Money.Amount.t option (** the merchant amount when it was converted *)
    ; auth_id : Ids.Auth_id.t option
    ; clearing_id : Ids.Clearing_id.t option
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { card_id : Ids.Card_id.t
  ; masked_pan : Masked_pan.t option
  ; from : Payments_core.Utc_time.t
  ; until : Payments_core.Utc_time.t (** exclusive *)
  ; lines : Line.t list
  ; spent : Money.Amount.t (** purchases and fees, positive *)
  ; refunded : Money.Amount.t
  ; pending : Money.Amount.t (** holds still open on the records given *)
  }
[@@deriving sexp_of]

(** Ignores records of other cards. [include_declined] defaults to [false]. *)
val create
  :  card:Card.t
  -> authorizations:Authorization.t list
  -> clearings:Settlement.Clearing.t list
  -> from:Time_ns.t
  -> until:Time_ns.t
  -> ?include_declined:bool
  -> unit
  -> t Or_error.t

(** A fixed-width table with totals. *)
val to_string_hum : t -> string
