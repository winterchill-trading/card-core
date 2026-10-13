(lang dune 3.17)

(name card-core)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/card-core))

(package
 (name card-core)
 (synopsis "Provider-neutral card lifecycle, authorization, clearing and ledger booking")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  zarith
  ppx_jane
  money
  ledger
  payments-core))
