open! Core

module Card_id =
  Payments_core.Opaque_id.Make
    (struct
      let module_name = "Card_core.Card_id"
    end)
    ()

module Auth_id =
  Payments_core.Opaque_id.Make
    (struct
      let module_name = "Card_core.Auth_id"
    end)
    ()

module Network_txn_id =
  Payments_core.Opaque_id.Make
    (struct
      let module_name = "Card_core.Network_txn_id"
    end)
    ()

module Clearing_id =
  Payments_core.Opaque_id.Make
    (struct
      let module_name = "Card_core.Clearing_id"
    end)
    ()

module Dispute_id =
  Payments_core.Opaque_id.Make
    (struct
      let module_name = "Card_core.Dispute_id"
    end)
    ()
