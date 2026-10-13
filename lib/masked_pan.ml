open! Core

module Last4 = struct
  module T = struct
    type t = string [@@deriving compare, equal, hash]

    let to_string t = t

    let of_string_or_error s =
      if String.length s = 4 && String.for_all s ~f:Char.is_digit
      then Ok s
      else
        (* The input is not echoed back: it may be a full card number. *)
        Or_error.error_s
          [%message
            "Masked_pan.Last4: expected exactly four digits"
              ~length:(String.length s : int)]
    ;;

    let of_string s = of_string_or_error s |> Or_error.ok_exn
    let sexp_of_t t = Sexp.Atom t

    let t_of_sexp = function
      | Sexp.Atom s -> of_string s
      | Sexp.List _ as sexp -> of_sexp_error "Last4: expected an atom" sexp
    ;;
  end

  include T

  let quickcheck_generator =
    Quickcheck.Generator.map (Int.gen_incl 0 9999) ~f:(sprintf "%04d")
  ;;

  let quickcheck_observer = String.quickcheck_observer
  let quickcheck_shrinker : t Quickcheck.Shrinker.t = Quickcheck.Shrinker.empty ()

  module Stable = struct
    module V1 = struct
      include T

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (String)
          (struct
            type nonrec t = t

            let to_binable t = t
            let of_binable = of_string
          end)
    end
  end
end

module Expiry = struct
  type t =
    { month : int
    ; year : int
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  module Wire = struct
    type nonrec t = t [@@deriving bin_io, sexp]
  end

  let create ~month ~year =
    if month < 1 || month > 12
    then Or_error.error_s [%message "Expiry: month must be 1 to 12" (month : int)]
    else if year < 2000 || year > 9999
    then Or_error.error_s [%message "Expiry: year must have four digits" (year : int)]
    else Ok { month; year }
  ;;

  let month t = t.month
  let year t = t.year
  let to_string t = sprintf "%02d/%02d" t.month (t.year % 100)

  let end_ t =
    let next_month, year = if t.month = 12 then 1, t.year + 1 else t.month + 1, t.year in
    Time_ns.of_date_ofday
      ~zone:Time_float.Zone.utc
      (Date.create_exn ~y:year ~m:(Month.of_int_exn next_month) ~d:1)
      Time_ns.Ofday.start_of_day
  ;;

  let is_expired t ~now = Time_ns.( >= ) now (end_ t)
  let of_wire (w : Wire.t) = create ~month:w.month ~year:w.year |> Or_error.ok_exn
  let t_of_sexp sexp = Wire.t_of_sexp sexp |> of_wire

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%map month = Int.gen_incl 1 12
    and year = Int.gen_incl 2024 2035 in
    { month; year }
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.unmap Int.quickcheck_observer ~f:(fun t ->
      (t.year * 100) + t.month)
  ;;

  let quickcheck_shrinker : t Quickcheck.Shrinker.t = Quickcheck.Shrinker.empty ()

  module Stable = struct
    module V1 = struct
      type nonrec t = t [@@deriving compare, equal, hash, sexp_of]

      let t_of_sexp = t_of_sexp

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (Wire)
          (struct
            type nonrec t = t

            let to_binable t = t
            let of_binable = of_wire
          end)
    end
  end
end

module Stable = struct
  module V1 = struct
    type t =
      { last4 : Last4.Stable.V1.t
      ; expiry : Expiry.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V1.t =
  { last4 : Last4.t
  ; expiry : Expiry.t
  }
[@@deriving sexp, compare, equal, hash, quickcheck]

let to_string t =
  sprintf "**** %s %s" (Last4.to_string t.last4) (Expiry.to_string t.expiry)
;;
