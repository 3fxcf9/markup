(lang dune 3.24)
(generate_opam_files true)

(name markup)

(package
 (name markup)
 (depends
  (ocaml (>= "5.5.0"))
  (dune (>= "3.24.0"))
  (ocaml-lua (>= "1.8"))
  (cmdliner (>= "2.1.1"))))
