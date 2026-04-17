(lang dune 3.21)

(name ppx_mixins)

(generate_opam_files true)

(source
 (github soteria-tools/ppx_mixins))

(authors "Sacha-Élie Ayoun <sachaayoun@gmail.com>")

(maintainers "Sacha-Élie Ayoun <sachaayoun@gmail.com>")

(license Apache-2.0)

(documentation https://github.com/soteria-tools/ppx_mixins)

(package
 (name ppx_mixins)
 (synopsis "A PPX for mixing module type signatures into a type declaration")
 (description
  "Expands `type my_type [@@mixins Mappable (key = string) + Eq] into `type t include Mappable with type t := my_type and type key = string; include Eq with type t := my_type`")
 (depends
  (ocaml
   (>= 4.14.0))
  (ppxlib
   (= "0.28.0"))
  (ocamlformat
   (and
    :with-test
    (= 0.29.0))))
 (tags
  (ppx metaprogramming)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
