(* Module types used as mixins in the tests *)

module type Printable = sig
  type t
  val to_string : t -> string
end

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Mappable = sig
  type t
  type key
  type value
  val get : key -> t -> value option
end

module type Weird = sig
  type t
  type u
  val inspect : u -> string
end

(* ── Test 1: single bare mixin ── *)
module type S1 = sig
  type my_type [@@mixins Printable]
end
(* Expands to:
   type my_type
   include Printable with type t := my_type *)

let _check_s1 =
  let module M : S1 = struct
    type my_type = int
    let to_string = string_of_int
  end in
  let _ : M.my_type -> string = M.to_string in
  ()

(* ── Test 4: explicit substitution on a *different* name — t := still injected ── *)
module type S4 = sig
  type my_type [@@mixins Weird (u := my_type)]
end
(* Expands to:
   type my_type
   include Weird with type u := my_type and type t := my_type *)

let _check_s4 =
  let module M : S4 = struct
    type my_type = float
    type u = my_type
    let inspect x = string_of_float x
  end in
  let _ : M.my_type -> string = M.inspect in
  ()

(* ── Test 4b: explicit t binding — no injection ── *)
module type S4b = sig
  type my_type [@@mixins Weird (t := my_type; u := my_type)]
end
(* Expands to:
   type my_type
   include Weird with type t := my_type and type u := my_type *)

let _check_s4b =
  let module M : S4b = struct
    type my_type = float
    type u = my_type
    let inspect x = string_of_float x
  end in
  let _ : M.my_type -> string = M.inspect in
  ()

(* ── Test 2: multiple bare mixins ── *)
module type S2 = sig
  type my_type [@@mixins Printable; Comparable]
end
(* Expands to:
   type my_type
   include Printable  with type t := my_type
   include Comparable with type t := my_type *)

let _check_s2 =
  let module M : S2 = struct
    type my_type = int
    let to_string = string_of_int
    let compare = Int.compare
  end in
  let _ : M.my_type -> string        = M.to_string in
  let _ : M.my_type -> M.my_type -> int = M.compare in
  ()

(* ── Test 3: equality constraint — key = string added alongside t := ── *)
module type S3 = sig
  type my_type [@@mixins Mappable (key = string; value = int)]
end
(* Expands to:
   type my_type
   include Mappable with type key = string and type value = int and type t := my_type *)

let _check_s3 =
  let module M : S3 = struct
    type my_type = (string * int) list
    type key   = string
    type value = int
    let get k m = List.assoc_opt k m
  end in
  let _ : M.key -> M.my_type -> M.value option = M.get in
  ()

(* ── Test 5: mixed equality + explicit substitution ── *)
module type S5 = sig
  type my_type [@@mixins Mappable (key = string; value = int; t := my_type)]
end

let _check_s5 =
  let module M : S5 = struct
    type my_type = (string * int) list
    type key   = string
    type value = int
    let get k m = List.assoc_opt k m
  end in
  let _ : M.key -> M.my_type -> M.value option = M.get in
  ()

let () = print_endline "All tests passed."
