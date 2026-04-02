(* ppx_mixins — inline mixin includes for OCaml signatures
   =========================================================

   This PPX rewrites a type declaration annotated with [@@mixins ...] inside a
   [sig] block into the type declaration followed by one [include M with ...]
   item per mixin.  For example:

     module type S = sig
       type my_type [@@mixins Printable; Mappable (key = string; value := int)]
     end

   expands to:

     module type S = sig
       type my_type
       include Printable with type t := my_type
       include Mappable  with type key = string
                          and type value := int
                          and type t := my_type
     end

   The attribute payload is a plain OCaml expression (single_expr_payload).
   Multiple mixins are separated by [;] at the top level (parsed by the OCaml
   parser as [Pexp_sequence]).  Per-mixin parameters are also [;]-separated
   inside parentheses.

   Constraint syntax within parameter lists:
     name = Type    →  with type name =  Type   (equality / sharing constraint)
     name := Type   →  with type name := Type   (destructive substitution)

   The transformation runs in three phases:

   Phase 1 — Parse
     Recursively flatten the [Pexp_sequence] tree produced by the OCaml parser
     into a [mixin list].  Each mixin carries a [module_path] (the longident of
     the module type) and a [constraint_ list] parsed from the optional argument.
     A bare [Constructor] with no argument yields an empty constraint list.

   Phase 2 — Inject default substitution
     Mixin module types conventionally name their primary type [t].  If the
     constraint list contains no binding (neither [Eq] nor [Subst]) for the
     name ["t"], a [Subst ("t", Lident type_name)] constraint is prepended
     automatically.  This connects the mixin's [t] to the annotated type
     without requiring the user to write [t := my_type] every time.
     If the user supplies an explicit [t = ...] or [t := ...] constraint, the
     automatic injection is suppressed.

   Phase 3 — Desugar
     Each [mixin] is turned into a [psig_include] node:
       [include <module_path> with <constraint> and ...]
     The original [psig_type] node is re-emitted without the [@@mixins]
     attribute, followed by all the generated [psig_include] nodes.

   The expansion is wired up as a global [Ast_traverse.map] registered via
   [Driver.V2.register_transformation], so it recurses into every [sig] block
   in both [.ml] and [.mli] files, including nested [sig]s inside structures.
*)

open Ppxlib

(* ── Intermediate representation ─────────────────────────────────────────── *)

(* A single type constraint in a mixin's parameter list. *)
type constraint_ =
  | Subst of string * longident  (* name := rhs  →  with type name := rhs *)
  | Eq    of string * longident  (* name =  rhs  →  with type name =  rhs *)

type mixin = {
  module_path : longident;
  constraints : constraint_ list;
}

(* ── Parsing ──────────────────────────────────────────────────────────────── *)

(* Parse one constraint expression of the form [name := rhs] or [name = rhs].
   Both [lhs] and [rhs] must be plain identifiers (possibly dotted for rhs). *)
let parse_constraint ~loc expr =
  match expr.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident (":=" | "=" as op); _ }; _ },
                [ Nolabel, { pexp_desc = Pexp_ident { txt = Lident name; _ }; _ };
                  Nolabel, { pexp_desc = Pexp_ident { txt = rhs; _ };          _ } ]) ->
    if op = ":=" then Subst (name, rhs) else Eq (name, rhs)
  | _ ->
    Location.raise_errorf ~loc:expr.pexp_loc
      "ppx_mixins: expected a constraint of the form 'name = Type' or \
       'name := Type'"

(* Flatten a [Pexp_sequence]-or-single expression into a list of constraints. *)
let rec parse_constraints ~loc expr =
  match expr.pexp_desc with
  | Pexp_sequence (a, b) ->
    parse_constraints ~loc a @ parse_constraints ~loc b
  | _ ->
    [ parse_constraint ~loc expr ]

(* Parse one mixin entry: [Constructor] or [Constructor (constraints...)]. *)
let parse_mixin ~loc expr =
  match expr.pexp_desc with
  | Pexp_construct ({ txt = module_path; _ }, arg_opt) ->
    let constraints = match arg_opt with
      | None   -> []
      | Some e -> parse_constraints ~loc e
    in
    { module_path; constraints }
  | _ ->
    Location.raise_errorf ~loc:expr.pexp_loc
      "ppx_mixins: expected a module type name (e.g. 'Printable' or \
       'Mappable (key = string)')"

(* Flatten the top-level sequence into a list of mixins. *)
let rec parse_mixins ~loc expr =
  match expr.pexp_desc with
  | Pexp_sequence (a, b) ->
    parse_mixins ~loc a @ parse_mixins ~loc b
  | _ ->
    [ parse_mixin ~loc expr ]

(* ── Phase 2: inject default substitution ────────────────────────────────── *)

(* If no [Subst] constraint is present, prepend [t := type_name] so that
   the desugaring phase can treat all mixins uniformly. *)
let inject_default_subst ~type_name mixin =
  let has_t_binding = List.exists (function
    | Subst ("t", _) | Eq ("t", _) -> true
    | _ -> false)
  in
  if has_t_binding mixin.constraints then mixin
  else { mixin with constraints = Subst ("t", Lident type_name) :: mixin.constraints }

(* ── Phase 3: desugar into a [psig_include] ──────────────────────────────── *)

(* Build the [type_declaration] node used as the RHS of both [Pwith_type] and
   [Pwith_typesubst].  The manifest is a plain type constructor from [rhs]. *)
let make_type_decl ~loc rhs =
  let open Ast_builder.Default in
  type_declaration ~loc
    ~name:{ txt = "t"; loc }   (* name is overridden by the caller anyway *)
    ~params:[] ~cstrs:[]
    ~kind:Ptype_abstract
    ~private_:Public
    ~manifest:(Some (ptyp_constr ~loc { txt = rhs; loc } []))

let constraint_to_with ~loc = function
  | Subst (name, rhs) ->
    Pwith_typesubst ({ txt = Lident name; loc }, make_type_decl ~loc rhs)
  | Eq (name, rhs) ->
    Pwith_type ({ txt = Lident name; loc }, make_type_decl ~loc rhs)

let mixin_to_sig_item ~loc mixin =
  let open Ast_builder.Default in
  let base  = pmty_ident ~loc { txt = mixin.module_path; loc } in
  let withs = List.map (constraint_to_with ~loc) mixin.constraints in
  let mty   = pmty_with ~loc base withs in
  psig_include ~loc (include_infos ~loc mty)

(* ── Attribute & expansion ───────────────────────────────────────────────── *)

let mixins_attr =
  Attribute.declare "mixins"
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload __)
    Fun.id

let expand_type_decl ~loc rec_flag td =
  match Attribute.get mixins_attr td with
  | None -> None
  | Some payload ->
    let type_name = td.ptype_name.txt in
    (* Phase 1: parse *)
    let mixins = parse_mixins ~loc payload in
    (* Phase 2: inject default [t := type_name] where no Subst is present *)
    let mixins = List.map (inject_default_subst ~type_name) mixins in
    (* Phase 3: desugar each mixin into an [include] signature item *)
    let include_items = List.map (mixin_to_sig_item ~loc) mixins in
    let clean_td =
      { td with ptype_attributes =
          List.filter (fun a -> a.attr_name.txt <> "mixins") td.ptype_attributes }
    in
    let type_item = Ast_builder.Default.psig_type ~loc rec_flag [ clean_td ] in
    Some (type_item :: include_items)

(* ── AST traversal ───────────────────────────────────────────────────────── *)

(* Recursively walk the AST, expanding [@@mixins ...] at every signature level. *)
let mapper =
  object
    inherit Ast_traverse.map as super

    method! signature sig_ =
      let sig_ = super#signature sig_ in
      List.concat_map
        (fun item ->
          match item.psig_desc with
          | Psig_type (rec_flag, [ td ]) ->
            (match expand_type_decl ~loc:item.psig_loc rec_flag td with
             | Some items -> items
             | None -> [ item ])
          | _ -> [ item ])
        sig_
  end

let () =
  Driver.V2.register_transformation "mixins"
    ~impl:(fun _ctxt -> mapper#structure)
    ~intf:(fun _ctxt -> mapper#signature)
