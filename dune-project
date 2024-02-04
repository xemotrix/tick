(lang dune 3.12)

(name tick)

(generate_opam_files true)

(source
 (github xemotrix/tick))

(authors "Chema Rodriguez")

(maintainers "Chema Rodriguez <")

(license MIT)

(package
 (name tick)
 (synopsis "Small language using OCaml and LLVM")
 (description "Small language using OCaml and LLVM")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
