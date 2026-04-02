# AGENTS.md

## Project overview

`ppx_mixins` is a ppxlib-based OCaml PPX rewriter.  It expands a
`[@@mixins ...]` attribute on a `sig` type declaration into a series of
`include M with type t := ...` items.

The entire implementation lives in one file: `lib/ppx_mixins.ml`.

## Build and test commands

```bash
# Build everything
dune build

# Run the test executable
dune exec test/test_ppx_mixins.exe

# Inspect the expanded AST for a file
dune exec -- ocamlfind ppx_mixins/ppx_mixins.exe -dump-ast <file.ml>

# Or use dune's built-in ppx output (shows expanded source)
dune build @check 2>&1
```

Tests are type-checked by the compiler; if the build succeeds the tests pass.
There is no separate test framework — correctness is verified by constructing
module implementations that satisfy the expanded signature.

## Repository layout

```
dune-project
lib/
  dune              # ppx_rewriter library
  ppx_mixins.ml     # the entire PPX implementation
test/
  dune              # test executable (uses ppx_mixins as preprocessor)
  test_ppx_mixins.ml
```

## Code style

- OCaml: standard Jane Street-ish style, 2-space indent.
- Section headers use the `(* ── Title ──...── *)` banner style already present
  in `lib/ppx_mixins.ml`.
- No external dependencies beyond `ppxlib`.
- Keep the implementation in the single file `lib/ppx_mixins.ml`; do not split
  into multiple modules unless the file grows substantially.

## Key design decisions

- The attribute payload is a **`single_expr_payload`** (an OCaml expression),
  not a signature payload.  This is because the OCaml parser rejects multiple
  `;`-separated items inside `[@@attr: ...]` signature payloads.
- Uppercase module type names (e.g. `Printable`) parse as `Pexp_construct` in
  expression context, not `Pexp_ident`.
- Multiple mixins and multiple per-mixin params are both delimited by `;`,
  which the parser renders as `Pexp_sequence`.
- The traversal uses `Ast_traverse.map` with `method! signature` calling
  `super#signature` **first** so that nested `sig` blocks are expanded before
  the outer level is processed.
- `Driver.V2.register_transformation` is used (not a deriver) because the
  attribute is `[@@mixins]`, not `[@@deriving mixins]`.

## Making changes

When editing `lib/ppx_mixins.ml`:
1. Run `dune build` to check for compilation errors.
2. Run `dune exec test/test_ppx_mixins.exe` to verify the tests still pass.
3. If you add a new constraint form or mixin syntax, add a corresponding test
   case in `test/test_ppx_mixins.ml` using the same pattern as the existing
   ones (define a module type with `[@@mixins ...]`, then check it with an
   inline module).

## What agents should NOT do

- Do not add a `[@@deriving ...]` interface or rename the attribute.
- Do not split `lib/ppx_mixins.ml` into multiple files without a good reason.
- Do not introduce dependencies beyond `ppxlib`.
