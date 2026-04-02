(lang dune 3.21)

(name ppx_mixins)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ppx_mixins)
 (synopsis "A PPX for mixing module type signatures into a type declaration")
 (description "Expands [@@mixins: include S1; include S2 with type k = int] on type declarations in signatures")
 (depends ocaml ppxlib)
 (tags
  (ppx metaprogramming)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
