(lang dune 2.7)

(name azureusage)

(version 0.1)

(license MIT)

(authors "Vladislav Prekel")

(maintainers "Vladislav Prekel")

(generate_opam_files true)

(package
 (name azureusage)
 (synopsis "")
 (description "")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  core
  js_of_ocaml
  bonsai
  cohttp-lwt-jsoo
  (ocaml-embed-file :build)
  (ppx_jane :build)
  (ocaml-lsp-server :with-test)
  (ocamlformat :with-test)
  (ocamlformat-rpc :with-test)
  (utop :with-test)
  (odoc :with-doc)))
