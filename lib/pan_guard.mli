(** Last line of defence against a card number arriving in free text: a merchant
    descriptor, a dispute note, a support comment.

    A candidate is a run of 13 to 19 digits, possibly grouped by single spaces or dashes,
    that passes the Luhn check (ISO/IEC 7812-1). Text that holds one is refused or
    masked before it is stored. *)

open! Core

(** The Luhn (mod 10) check over a string of digits. *)
val luhn_valid : string -> bool

val contains_pan : string -> bool

(** Replaces every candidate with its last four digits behind asterisks:
    ["card 4111 1111 1111 1111 declined"] becomes ["card ************1111 declined"]. *)
val scrub : string -> string

(** [Ok s] when [s] holds no candidate; the error does not echo [s]. *)
val check : field:string -> string -> string Or_error.t
