(** Identifiers. Each is its own type, so an authorization id cannot be passed where a
    card id is expected. They are opaque strings chosen by the provider or by us; the
    ledger checks them again when they become part of a hold id or an idempotency key. *)

open! Core

(** A card, as the provider names it. *)
module Card_id : Payments_core.Opaque_id.S

(** One authorization message we answered or were told about. *)
module Auth_id : Payments_core.Opaque_id.S

(** The scheme's transaction identifier: links an authorization with its increments,
    reversals and clearings. *)
module Network_txn_id : Payments_core.Opaque_id.S

(** One clearing (presentment) record, debit or credit. *)
module Clearing_id : Payments_core.Opaque_id.S

module Dispute_id : Payments_core.Opaque_id.S
