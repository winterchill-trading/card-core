open! Core

let luhn_valid digits =
  let n = String.length digits in
  n > 0
  && String.for_all digits ~f:Char.is_digit
  &&
  let sum = ref 0 in
  for i = 0 to n - 1 do
    let d = Char.get_digit_exn digits.[n - 1 - i] in
    let d = if i % 2 = 1 then if d * 2 > 9 then (d * 2) - 9 else d * 2 else d in
    sum := !sum + d
  done;
  !sum % 10 = 0
;;

(* Finds digit runs where single spaces or dashes may separate groups, and returns
   (start, stop_exclusive, digits) for each run. *)
let runs s =
  let n = String.length s in
  let rec scan i acc =
    if i >= n
    then List.rev acc
    else if not (Char.is_digit s.[i])
    then scan (i + 1) acc
    else (
      let buf = Buffer.create 19 in
      let rec extend j last_digit_end =
        if j < n && Char.is_digit s.[j]
        then (
          Buffer.add_char buf s.[j];
          extend (j + 1) (j + 1))
        else if j + 1 < n
                && (Char.equal s.[j] ' ' || Char.equal s.[j] '-')
                && Char.is_digit s.[j + 1]
        then extend (j + 1) last_digit_end
        else last_digit_end
      in
      let stop = extend i i in
      scan stop ((i, stop, Buffer.contents buf) :: acc))
  in
  scan 0 []
;;

(* Only whole runs are checked. Checking every 13 to 19 digit window of a longer run
   would flag about one in ten long reference numbers, since a random window passes the
   Luhn check one time in ten. *)
let is_candidate digits =
  let n = String.length digits in
  n >= 13 && n <= 19 && luhn_valid digits
;;

let contains_pan s = List.exists (runs s) ~f:(fun (_, _, digits) -> is_candidate digits)

let scrub s =
  let buf = Buffer.create (String.length s) in
  let pos =
    List.fold (runs s) ~init:0 ~f:(fun pos (start, stop, digits) ->
      Buffer.add_string buf (String.sub s ~pos ~len:(start - pos));
      if is_candidate digits
      then (
        let n = String.length digits in
        Buffer.add_string buf (String.make (n - 4) '*');
        Buffer.add_string buf (String.sub digits ~pos:(n - 4) ~len:4))
      else Buffer.add_string buf (String.sub s ~pos:start ~len:(stop - start));
      stop)
  in
  Buffer.add_string buf (String.sub s ~pos ~len:(String.length s - pos));
  Buffer.contents buf
;;

let check ~field s =
  if contains_pan s
  then Or_error.error_s [%message "text looks like it contains a card number" field]
  else Ok s
;;
