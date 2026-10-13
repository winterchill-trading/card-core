open! Core

type t =
  { token : string
  ; expires_at : Time_ns.t
  }

let sexp_of_t _ = Sexp.Atom "<pan-display-token>"

let of_provider ~token ~expires_at =
  if String.is_empty token
  then Or_error.error_string "Pan_display_token: empty token"
  else Ok { token; expires_at }
;;

let expires_at t = t.expires_at
let is_expired t ~now = Time_ns.( >= ) now t.expires_at
let reveal_for_ui t = t.token
