(lang dune 3.19)
(using menhir 2.1)

(name LambdaPC)

(generate_opam_files true)

(source
 (github jpaykin/LambdaPC-ocaml))

(authors "Jennifer Paykin <jpaykin@gmail.com>")
(maintainers "Jennifer Paykin <jpaykin@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name LambdaPC)
 (synopsis "OCaml implementation of LambdaPC")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
