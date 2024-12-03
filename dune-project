(lang dune 3.16)

(generate_opam_files true)

(authors "Anh Do" "Spencer Ye")

(maintainers "anhddo93@gmail.com")

(package
 (name rllib)
 (allow_empty)
 (synopsis "An OCaml library for Helpful Helloing")
 (description "A longer description")
 (depends
  (ounit2
   (>= 2.2.7))
  ; for each opam dependency list the version
  (core
   (>= 0.17.1))
  ; `opam list core` will display which version of core you have
  ))
