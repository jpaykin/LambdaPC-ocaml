module Type :
  sig
    type t = { loc : Ident.Loc.t option; node : node; }
    and node = Unit | Sum of t * t | Arrow of t * t

    val t_of_node : node -> t
    val string_of_t : t -> string
    val pretty : t -> string
  end
module Expr :
  sig
    type t = { loc : Ident.Loc.t option; ty : Type.t option; node : node; }
    and node =
        Var of Ident.Ident.t
      | Zero
      | Const of int
      | Plus of t * t
      | Scale of t * t
      | Pair of t * t
      | Case of { scrut : t; x1 : Ident.Ident.t; a1 : t; x2 : Ident.Ident.t;
          a2 : t;
        }
      | Lambda of { x : Ident.Ident.t; tp : Type.t; body : t; }
      | App of t * t
      | Let of { x : Ident.Ident.t; a : t; body : t; }

    val t_of_node : ?loc:(Ident.Loc.t option)-> ?ty:(Type.t option) -> node -> t
    val string_of_t : t -> string
    val pretty_string_of_t : t -> string
    val subst : Ident.Ident.t -> t -> t -> t
    val rename_var : Ident.Ident.t -> Ident.Ident.t -> t -> t
    val map : (int -> int) -> t -> t
    val update_env : t -> unit
    val alpha_equiv : t -> t -> bool
  end
