open! Core

module Stable = struct
  module V1 = struct
    module Wire = struct
      type t =
        { name : string
        ; mcc : Mcc.Stable.V1.t
        ; country : Country.Stable.V1.t
        ; amount : Money.Amount.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end

    type t = Wire.t =
      { name : string
      ; mcc : Mcc.t
      ; country : Country.t
      ; amount : Money.Amount.t
      }
    [@@deriving compare, equal]

    let validate (t : t) =
      let open Or_error.Let_syntax in
      let%bind _ = Pan_guard.check ~field:"merchant name" t.name in
      if Money.Amount.is_negative t.amount
      then Or_error.error_s [%message "Merchant: negative amount" (t.amount : Money.Amount.t)]
      else Ok t
    ;;

    let sexp_of_t = Wire.sexp_of_t
    let t_of_sexp sexp = Wire.t_of_sexp sexp |> validate |> Or_error.ok_exn

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Wire)
        (struct
          type nonrec t = t

          let to_binable t = t
          let of_binable w = validate w |> Or_error.ok_exn
        end)
  end
end

include Stable.V1

let create ~name ~mcc ~country ~amount = validate { name; mcc; country; amount }
let with_amount t amount = validate { t with amount }
