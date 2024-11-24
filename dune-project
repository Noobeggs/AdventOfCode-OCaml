(lang dune 3.16)

(name aoc)

(generate_opam_files true)

(source
 (github Noobeggs/AdventOfCode-OCaml))

(authors "Noobeggs")

(maintainers "Noobeggs")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune cohttp cohttp-lwt re angstrom ppx-deriving (ocamlformat :dev) (lsp :dev))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
