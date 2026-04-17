Test 1: single bare mixin -- t is auto-substituted

  $ cat > s1.ml << 'EOF'
  > module type Printable = sig
  >   type t
  >   val to_string : t -> string
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Printable]
  > end
  > EOF
  $ ./standalone.exe --impl s1.ml
  module type Printable  = sig type t val to_string : t -> string end
  module type S  =
    sig type my_type include Printable with type  t :=  my_type end

Test 2: multiple bare mixins -- each gets its own include with t := my_type

  $ cat > s2.ml << 'EOF'
  > module type Printable = sig
  >   type t
  >   val to_string : t -> string
  > end
  > 
  > module type Comparable = sig
  >   type t
  >   val compare : t -> t -> int
  > end
  > 
  > module type S = sig
  >   type my_type
  >   [@@mixins
  >     Printable;
  >     Comparable]
  > end
  > EOF
  $ ./standalone.exe --impl s2.ml
  module type Printable  = sig type t val to_string : t -> string end
  module type Comparable  = sig type t val compare : t -> t -> int end
  module type S  =
    sig
      type my_type
      include Printable with type  t :=  my_type
      include Comparable with type  t :=  my_type
    end

Test 3: equality constraints -- key and value are kept as sharing constraints, t is injected

  $ cat > s3.ml << 'EOF'
  > module type Mappable = sig
  >   type t
  >   type key
  >   type value
  >   val get : key -> t -> value option
  > end
  > 
  > module type S = sig
  >   type my_type
  >   [@@mixins
  >     Mappable
  >       (key = string;
  >        value = int)]
  > end
  > EOF
  $ ./standalone.exe --impl s3.ml
  module type Mappable  =
    sig type t type key type value val get : key -> t -> value option end
  module type S  =
    sig
      type my_type
      include
        Mappable with type  t :=  my_type and type  key =  string and type
           value =  int
    end

Test 4: substitution on a different name -- t is still auto-injected alongside u

  $ cat > s4.ml << 'EOF'
  > module type Weird = sig
  >   type t
  >   type u
  >   val inspect : u -> string
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Weird (u := my_type)]
  > end
  > EOF
  $ ./standalone.exe --impl s4.ml
  module type Weird  = sig type t type u val inspect : u -> string end
  module type S  =
    sig
      type my_type
      include Weird with type  t :=  my_type and type  u :=  my_type
    end

Test 4b: explicit t binding -- auto-injection is suppressed

  $ cat > s4b.ml << 'EOF'
  > module type Weird = sig
  >   type t
  >   type u
  >   val inspect : u -> string
  > end
  > 
  > module type S = sig
  >   type my_type
  >   [@@mixins
  >     Weird
  >       (t := my_type;
  >        u := my_type)]
  > end
  > EOF
  $ ./standalone.exe --impl s4b.ml
  module type Weird  = sig type t type u val inspect : u -> string end
  module type S  =
    sig
      type my_type
      include Weird with type  t :=  my_type and type  u :=  my_type
    end

Test 5: mixed equality and explicit substitution -- explicit t suppresses injection

  $ cat > s5.ml << 'EOF'
  > module type Mappable = sig
  >   type t
  >   type key
  >   type value
  >   val get : key -> t -> value option
  > end
  > 
  > module type S = sig
  >   type my_type
  >   [@@mixins
  >     Mappable
  >       (key = string;
  >        value = int;
  >        t := my_type)]
  > end
  > EOF
  $ ./standalone.exe --impl s5.ml
  module type Mappable  =
    sig type t type key type value val get : key -> t -> value option end
  module type S  =
    sig
      type my_type
      include
        Mappable with type  key =  string and type  value =  int and type  t :=
           my_type
    end

Test 6: single-parameter type constructor on the RHS (int list)

  $ cat > s6.ml << 'EOF'
  > module type Container = sig
  >   type t
  >   type elt
  >   val mem : elt -> t -> bool
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Container (elt = int list)]
  > end
  > EOF
  $ ./standalone.exe --impl s6.ml
  module type Container  = sig type t type elt val mem : elt -> t -> bool end
  module type S  =
    sig
      type my_type
      include Container with type  t :=  my_type and type  elt =  int list
    end

Test 7: multi-parameter type constructor on the RHS ((int, string) result)

  $ cat > s7.ml << 'EOF'
  > module type Mapper = sig
  >   type t
  >   type result_
  >   val run : t -> result_
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Mapper (result_ = (int, string) result)]
  > end
  > EOF
  $ ./standalone.exe --impl s7.ml
  module type Mapper  = sig type t type result_ val run : t -> result_ end
  module type S  =
    sig
      type my_type
      include
        Mapper with type  t :=  my_type and type  result_ = 
          (int, string) result
    end

Test 8: dotted module path as mixin name (M.Make style)

  $ cat > s8.ml << 'EOF'
  > module Ord = struct
  >   module type S = sig
  >     type t
  >     val compare : t -> t -> int
  >   end
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Ord.S]
  > end
  > EOF
  $ ./standalone.exe --impl s8.ml
  module Ord =
    struct module type S  = sig type t val compare : t -> t -> int end end
  module type S  = sig type my_type include Ord.S with type  t :=  my_type end

Test 9: the unit type as a constraint RHS

  $ cat > s9.ml << 'EOF'
  > module type Callback = sig
  >   type t
  >   type ret
  >   val call : t -> ret
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Callback (ret = unit)]
  > end
  > EOF
  $ ./standalone.exe --impl s9.ml
  module type Callback  = sig type t type ret val call : t -> ret end
  module type S  =
    sig
      type my_type
      include Callback with type  t :=  my_type and type  ret =  unit
    end

Test 10: multiple mixins with constraints

  $ cat > s10.ml << 'EOF'
  > module type Printable = sig
  >   type t
  >   val to_string : t -> string
  > end
  > 
  > module type Mappable = sig
  >   type t
  >   type key
  >   type value
  >   val get : key -> t -> value option
  > end
  > 
  > module type S = sig
  >   type my_type
  >   [@@mixins
  >     Printable;
  >     Mappable (key = string; value = int)]
  > end
  > EOF
  $ ./standalone.exe --impl s10.ml
  module type Printable  = sig type t val to_string : t -> string end
  module type Mappable  =
    sig type t type key type value val get : key -> t -> value option end
  module type S  =
    sig
      type my_type
      include Printable with type  t :=  my_type
      include
        Mappable with type  t :=  my_type and type  key =  string and type
           value =  int
    end

-- Error cases --

Test E1: mixin payload is not a module-type name (lowercase ident)

  $ cat > e1.ml << 'EOF'
  > module type S = sig
  >   type my_type [@@mixins printable]
  > end
  > EOF
  $ ./standalone.exe --impl e1.ml
  File "e1.ml", line 2, characters 25-34:
  2 |   type my_type [@@mixins printable]
                               ^^^^^^^^^
  Error: ppx_mixins: expected a module type name (e.g. 'Printable' or 'Mappable (key = string)')
  [1]

Test E2: constraint has no operator -- bare name instead of name = Type

  $ cat > e2.ml << 'EOF'
  > module type Printable = sig
  >   type t
  >   val to_string : t -> string
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Printable (string)]
  > end
  > EOF
  $ ./standalone.exe --impl e2.ml
  File "e2.ml", line 7, characters 35-43:
  7 |   type my_type [@@mixins Printable (string)]
                                         ^^^^^^^^
  Error: ppx_mixins: expected a constraint of the form 'name = Type' or 'name := Type'
  [1]

Test E3: unsupported tuple type on RHS -- tuples are ambiguous with multiplication

  $ cat > e3.ml << 'EOF'
  > module type Pair = sig
  >   type t
  >   type pair
  >   val make : t -> t -> pair
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Pair (pair = int * string)]
  > end
  > EOF
  $ ./standalone.exe --impl e3.ml
  File "e3.ml", line 8, characters 38-50:
  8 |   type my_type [@@mixins Pair (pair = int * string)]
                                            ^^^^^^^^^^^^
  Error: ppx_mixins: unsupported type expression; expected a type constructor or type application
  [1]

Test E4: constraint uses -> (function type) which is not supported

  $ cat > e4.ml << 'EOF'
  > module type Fn = sig
  >   type t
  >   type f
  >   val apply : f -> t
  > end
  > 
  > module type S = sig
  >   type my_type [@@mixins Fn (f = int -> string)]
  > end
  > EOF
  $ ./standalone.exe --impl e4.ml
  File "e4.ml", line 8, characters 37-39:
  8 |   type my_type [@@mixins Fn (f = int -> string)]
                                           ^^
  Error: Syntax error: ) expected
  File "e4.ml", line 8, characters 28-29:
  8 |   type my_type [@@mixins Fn (f = int -> string)]
                                  ^
    This ( might be unmatched
  [1]

Test E5: empty payload -- [@@mixins] with no argument

  $ cat > e5.ml << 'EOF'
  > module type S = sig
  >   type my_type [@@mixins]
  > end
  > EOF
  $ ./standalone.exe --impl e5.ml
  File "e5.ml", line 2, characters 18-24:
  2 |   type my_type [@@mixins]
                        ^^^^^^
  Error: :: expected
  [1]
