(** A payment card and its lifecycle.

    {v
      Requested --> Active <--> Frozen
          |           |  \       / |
          |           |   Blocked  |
          |           |      |     |
          |           +--> Expired <+
          |                  |
          +------------> Destroyed
    v}

    [Requested]: ordered, not usable yet (a physical card in the post). [Active]: usable.
    [Frozen]: paused by the cardholder, reversible. [Blocked]: stopped for good by the
    issuer or the cardholder (lost, stolen, fraud). [Expired]: past its expiry date.
    [Destroyed]: closed and gone; terminal. Every state except [Active] declines
    authorizations (see {!Status.decline_code}).

    The card refers to its funding account in the ledger; holds and captures for its
    authorizations go there. The library stores nothing itself: callers keep cards in
    their own store and pass them in. *)

open! Core

module Kind : sig
  type t =
    | Virtual
    | Physical
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

module Status : sig
  type t =
    | Requested
    | Active
    | Frozen
    | Blocked
    | Expired
    | Destroyed
  [@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

  (** The allowed transitions: [(from, [to_; ...])] for every state. *)
  val transitions : (t * t list) list

  val can_transition : from:t -> to_:t -> bool
  val is_terminal : t -> bool

  (** How an authorization on a card in this state is declined: [None] for [Active]; 57
      for [Requested] and [Frozen]; 62 for [Blocked]; 54 for [Expired]; 46 for
      [Destroyed]. *)
  val decline_code : t -> Response_code.t option

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

type t = private
  { id : Ids.Card_id.t
  ; holder : string (** an opaque customer id, never a name *)
  ; account : Ledger.Account_id.t (** the funding account *)
  ; currency : Money.Currency.t (** billing currency *)
  ; kind : Kind.t
  ; status : Status.t
  ; masked_pan : Masked_pan.t option (** known once the provider has issued it *)
  ; limits : Limits.t
  ; created_at : Payments_core.Utc_time.t
  ; updated_at : Payments_core.Utc_time.t
  }
[@@deriving sexp_of, compare, equal]

(** A new card in [Requested]. [limits] default to {!Limits.unlimited} and must be in
    [currency]. *)
val request
  :  id:Ids.Card_id.t
  -> holder:string
  -> account:Ledger.Account_id.t
  -> currency:Money.Currency.t
  -> kind:Kind.t
  -> ?limits:Limits.t
  -> at:Time_ns.t
  -> unit
  -> t Or_error.t

(** Fails when the transition is not in {!Status.transitions}; a transition to the current
    state is a no-op, so a repeated webhook is harmless. *)
val transition : t -> to_:Status.t -> at:Time_ns.t -> t Or_error.t

val activate : t -> masked_pan:Masked_pan.t -> at:Time_ns.t -> t Or_error.t
val freeze : t -> at:Time_ns.t -> t Or_error.t
val unfreeze : t -> at:Time_ns.t -> t Or_error.t
val block : t -> at:Time_ns.t -> t Or_error.t
val destroy : t -> at:Time_ns.t -> t Or_error.t

(** Fails on a terminal card or limits in another currency. *)
val set_limits : t -> Limits.t -> at:Time_ns.t -> t Or_error.t

(** Why an authorization at [now] must be declined because of the card itself: its status,
    or an [Active] card past its expiry date (54) whose status has not caught up yet. *)
val unusable : t -> now:Time_ns.t -> Response_code.t option

(** [Expired] when an [Active] or [Frozen] card is past its expiry date at [now]. *)
val expire_if_due : t -> now:Time_ns.t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
