(** A short-lived handle that lets the cardholder's app show the full card number, CVV and
    PIN through the provider's hosted UI component, without the values passing through our
    servers.

    The token is a bearer secret for a few minutes: it prints as [<pan-display-token>] in
    sexps and has no serializer, so it cannot end up in a log, a journal or a test
    expectation. Hand it to the app with {!reveal_for_ui} and forget it. *)

open! Core

type t [@@deriving sexp_of]

(** Wraps what the provider returned, with the time it stops working. Fails on an empty
    token. *)
val of_provider : token:string -> expires_at:Time_ns.t -> t Or_error.t

val expires_at : t -> Time_ns.t
val is_expired : t -> now:Time_ns.t -> bool

(** The raw token, for the one response that passes it to the app's UI component. *)
val reveal_for_ui : t -> string
