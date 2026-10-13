(** A cardholder's dispute of a cleared transaction (a chargeback), following the card
    scheme's stages.

    {v
      Filed --> Response (acquirer, 30 d; categories 12 and 13 only)
        |            |
        |            v
        +---> Pre_arbitration (30 d) --> Pre_arbitration_response (30 d) --> Arbitration (10 d)
    v}

    Fraud (10) and authorization (11) disputes skip the merchant's response and go
    straight to pre-arbitration if the acquirer contests. Any open stage may close as
    [Won] (the cardholder keeps the money), [Lost] or [Withdrawn].

    A card-absent fraud dispute cannot be opened when 3-D Secure shifted liability to
    the issuer ({!Three_ds.liability_shift}): the loss is ours, not the merchant's. *)

open! Core

module Category : sig
  type t =
    | Fraud (** 10 *)
    | Authorization (** 11 *)
    | Processing_error (** 12 *)
    | Consumer (** 13 *)
  [@@deriving sexp, compare, equal, hash, enumerate]

  (** 10, 11, 12, 13. *)
  val number : t -> int
end

module Reason : sig
  type t =
    | Card_absent_fraud (** 10.4 *)
    | Declined_authorization (** 11.2 *)
    | No_authorization (** 11.3 *)
    | Incorrect_amount (** 12.5 *)
    | Duplicate_processing (** 12.6 *)
    | Not_received (** 13.1 *)
    | Credit_not_processed (** 13.6 *)
  [@@deriving sexp, compare, equal, hash, enumerate]

  val category : t -> Category.t

  (** ["10.4"]. *)
  val code : t -> string
end

module Stage : sig
  type t =
    | Filed
    | Response
    | Pre_arbitration
    | Pre_arbitration_response
    | Arbitration
  [@@deriving sexp, compare, equal, hash, enumerate]

  (** 30 days for every stage but arbitration (10 days). *)
  val time_limit : t -> Time_ns.Span.t
end

module Outcome : sig
  type t =
    | Won (** decided for the cardholder: the amount is credited back *)
    | Lost
    | Withdrawn
  [@@deriving sexp, compare, equal, hash, enumerate]
end

module Status : sig
  type t =
    | Open of Stage.t
    | Closed of Outcome.t
  [@@deriving sexp, compare, equal]
end

module Evidence : sig
  type t =
    { kind : string (** e.g. [receipt], [proof_of_delivery], [cardholder_letter] *)
    ; description : string (** checked by {!Pan_guard}; never a card number *)
    ; at : Payments_core.Utc_time.t
    }
  [@@deriving sexp, compare, equal]
end

type t = private
  { id : Ids.Dispute_id.t
  ; card_id : Ids.Card_id.t
  ; clearing_id : Ids.Clearing_id.t
  ; amount : Money.Amount.t (** billing currency, at most the cleared amount *)
  ; reason : Reason.t
  ; status : Status.t
  ; evidence : Evidence.t list (** oldest first *)
  ; opened_at : Payments_core.Utc_time.t
  ; stage_started_at : Payments_core.Utc_time.t
  }
[@@deriving sexp_of, compare, equal]

(** 120 days from the clearing's processing date: the window for most reasons. *)
val filing_window : Time_ns.Span.t

(** Fails outside the filing window, for a credit clearing, for an amount that is not
    positive or above the clearing, or for card-absent fraud when [three_ds] shifted
    liability to the issuer. *)
val open_
  :  id:Ids.Dispute_id.t
  -> clearing:Settlement.Clearing.t
  -> ?three_ds:Three_ds.t
  -> scheme:Three_ds.Scheme.t
  -> reason:Reason.t
  -> amount:Money.Amount.t
  -> at:Time_ns.t
  -> unit
  -> t Or_error.t

val add_evidence : t -> kind:string -> description:string -> at:Time_ns.t -> t Or_error.t

(** Moves an open dispute to the next stage (see the diagram). *)
val advance : t -> to_:Stage.t -> at:Time_ns.t -> t Or_error.t

val close : t -> Outcome.t -> at:Time_ns.t -> t Or_error.t

(** When the current stage's time runs out; [None] once closed. *)
val deadline : t -> Time_ns.t option

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
