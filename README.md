# ppx_mixins

A ppxlib-based OCaml PPX that lets you compose module-type signatures inline
using a `[@@mixins ...]` attribute on a type declaration.

## Motivation

OCaml's `include M with type t := foo` idiom is the standard way to "mix in"
the interface of a module type while substituting the primary type.  Writing it
by hand is repetitive, especially when several mixins share the same `t :=`
substitution.  `ppx_mixins` lets you express this intent concisely:

```ocaml
module type S = sig
  type my_type [@@mixins Printable + Comparable]
end
```

expands to:

```ocaml
module type S = sig
  type my_type
  include Printable  with type t := my_type
  include Comparable with type t := my_type
end
```

## Installation

Add `ppx_mixins` to your project's dependencies and preprocessors in `dune`:

```
(library
 (name my_lib)
 (preprocess (pps ppx_mixins)))
```

## Syntax

```
type <name> [@@mixins <mixin> [+ <mixin> ...]]
```

where each `<mixin>` is:

```
ModuleTypeName
ModuleTypeName (param [; param ...])
```

and each `param` is one of:

| Syntax         | Meaning                                             |
|----------------|-----------------------------------------------------|
| `name = Type`  | equality constraint (`with type name = Type`)       |
| `name := Type` | destructive substitution (`with type name := Type`) |

`Type` can be any of:

| Form                   | Example                    |
|------------------------|----------------------------|
| plain constructor      | `string`, `My_module.t`    |
| `unit`                 | `unit`                     |
| single-param applied   | `int list`, `string option` |
| multi-param applied    | `(int, string) result`     |

Multiple mixins are separated by `+`.  Multiple params within a mixin's
parentheses are separated by `;`.

## Automatic `t` substitution

Mixin module types conventionally name their primary type `t`.  If you do not
provide any constraint that binds `t` (either `t = ...` or `t := ...`),
`ppx_mixins` automatically prepends `type t := <name>` for you.  To suppress
this, supply an explicit `t` constraint:

```ocaml
type my_type [@@mixins Weird (t := my_type; u := my_type)]
(* no extra  t :=  is injected *)
```

## Examples

### Single bare mixin

```ocaml
module type S = sig
  type my_type [@@mixins Printable]
end
(* expands to:
   type my_type
   include Printable with type t := my_type *)
```

### Multiple bare mixins

```ocaml
module type S = sig
  type my_type [@@mixins Printable + Comparable]
end
(* expands to:
   type my_type
   include Printable  with type t := my_type
   include Comparable with type t := my_type *)
```

### Equality constraints

```ocaml
module type S = sig
  type my_type [@@mixins Mappable (key = string; value = int)]
end
(* expands to:
   type my_type
   include Mappable with type key = string
                     and type value = int
                     and type t := my_type *)
```

### Mixed equality and substitution with explicit `t`

```ocaml
module type S = sig
  type my_type [@@mixins Mappable (key = string; value = int; t := my_type)]
end
(* expands to:
   type my_type
   include Mappable with type key = string
                     and type value = int
                     and type t := my_type *)
```

### Parameterised constraint RHS

```ocaml
module type S = sig
  type my_type [@@mixins Container (elt = int list)]
end
(* expands to:
   type my_type
   include Container with type elt = int list and type t := my_type *)
```

Multi-parameter constructors use the same parenthesised tuple syntax as in
type expressions:

```ocaml
type my_type [@@mixins Mapper (result_ = (int, string) result)]
```

## Shorthand module type syntax

When the primary type in the expanded signature should simply be named `t`,
you can skip the `sig ... end` wrapper entirely and write:

```ocaml
module type S = [%mixins Printable + Comparable]
```

which expands to:

```ocaml
module type S = sig
  type t
  include Printable  with type t := t
  include Comparable with type t := t
end
```

All the same mixin syntax (params, constraints, `+`-separated lists) works
identically to the `[@@mixins]` form.  The only difference is that the
implicit primary type is always `t` -- there is no way to choose a different
name with the shorthand.  If you need a different primary type name, use the
full `[@@mixins]` attribute form.

## Implementation overview

The rewriter is implemented as a global `Ast_traverse.map` registered via
`Driver.V2.register_transformation`.  It walks every `sig` block in both `.ml`
and `.mli` files and runs a three-phase pipeline on each annotated type
declaration:

1. **Parse** — flatten the `Pexp_apply "+"` expression tree into a `mixin list`.
2. **Inject** — prepend `t := <type_name>` to any mixin that has no `t` binding.
3. **Desugar** — emit `psig_type` (without the attribute) followed by one
   `psig_include` per mixin.

See [`lib/ppx_mixins.ml`](lib/ppx_mixins.ml) for the full documented source.

## Limitations

- Tuple types (`int * string`) and function types (`int -> string`) on the RHS
  of a constraint are not supported.  Wrap them in a type alias if needed.
- Only `sig` items are rewritten; `[@@mixins]` on a type inside a `struct` is
  not meaningful and will be left untouched (ppxlib will warn about an unused
  attribute).
