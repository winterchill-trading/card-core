(** The issuer's answer to one authorization request: a pure function of the card, its
    limits and recent spend, the funding account's available balance, the SCA state and an
    optional risk hook.

    Checks run in a fixed order and the first failure decides the response code:

    + the card: unknown (14), not active (57, 62, 54 or 46), past its expiry (54);
    + the amount is in the card's billing currency (else 05);
    + an incremental request has an open authorization to add to (else 05);
    + card controls and limits ({!Limits.check}: 57, 61, 65);
    + funds (51), or a partial approval of the available balance when the merchant
      supports it;
    + strong customer authentication ({!Card_sca}: 1A to ask for it, 05 when 3-D Secure
      failed);
    + the risk hook: [Review] and [Block] both decline with 05, so a cardholder cannot
      tell a risk decline from any other.

    Funds come before SCA so a cardholder is never challenged for a payment that would be
    declined anyway. Advices are not decided here: they arrive decided
    ({!Authorization.advice}).

    It allocates little and does no I/O; a provider's real-time authorization webhook
    typically allows about 2 seconds, and this leaves nearly all of it to the caller's
    store reads. *)

open! Core

module Risk : sig
  type verdict =
    | Allow
    | Review of string (** the rule or model that asked for review *)
    | Block of string
  [@@deriving sexp, compare, equal]
end

module Input : sig
  type t =
    { card : Card.t
    ; usage : Limits.Usage.t (** {!Authorization.usage} of the card's authorizations *)
    ; available : Money.Amount.t
    (** funding account available balance, billing currency *)
    ; sca_policy : Card_sca.Policy.t
    ; sca_counters : Payments_core.Sca.Counters.t
    ; request : Authorization.Request.t
    ; open_auth : Authorization.t option
    (** for an incremental request: the authorization with the same network transaction id *)
    }
  [@@deriving sexp_of]
end

type t =
  | Approve of
      { amount : Money.Amount.t
      ; partial : bool
      ; sca : Card_sca.Outcome.t
      }
  | Decline of Decline_reason.t
[@@deriving sexp_of, compare, equal]

val decide : ?risk:(Input.t -> Risk.verdict) -> Input.t -> now:Time_ns.t -> t
val response_code : t -> Response_code.t

(** Applies the decision to the pending authorization built from the same request
    ({!Authorization.pending}), with the SCA evidence the ledger keeps. *)
val apply
  :  t
  -> Authorization.t
  -> request:Authorization.Request.t
  -> Authorization.t Or_error.t
