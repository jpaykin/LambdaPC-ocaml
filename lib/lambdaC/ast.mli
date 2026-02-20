(*open Core.Scalars*)
open Interface.Ident

module Type : sig
  type t = { loc : Loc.t; node : node }
  and node =
    | Unit
    | Sum of t * t
    | Arrow of t * t
    
  val string_of_t : t -> string
end

(*
module Variable : Map.OrderedType with type t = int
module VariableMap : Map.S with type key = Variable.t
module VariableSet : sig
  include Set.S with type elt = Variable.t
  val exists_usage_subset : t -> (t -> bool) -> bool
end

module VariableEnvironment : sig
  type t
  val init : t
  val fresh : t -> Variable.t
  val update : Variable.t -> t -> unit
end 
*)

module Expr : sig
  type t = { loc : Loc.t; ty : Type.t option; node : node }
  and node =
    | Var of Ident.t
    | Zero of Type.t
    | Const of int
    | Plus of t * t
    | Scale of t * t
    | Pair of t * t
    | Case of
        { scrut : t
        ; x1 : Ident.t
        ; a1 : t
        ; x2 : Ident.t
        ; a2 : t
        }
    | Lambda of { x : Ident.t; tp : Type.t; body : t }
    | App of t * t
    | Let of { x : Ident.t; lhs : t; body : t }
    | Annot of t * Type.t

  val string_of_t : t -> string
  val pretty_string_of_t : t -> string
  val subst : Ident.t -> t -> t -> t
  val rename_var : Ident.t -> Ident.t -> t -> t
  (*
  val map : (int -> int) -> t -> t
  val update_env : VariableEnvironment.t -> t -> unit
  val alpha_equiv : t -> t -> bool
  *)
end


(*
module HOAS : sig
  val var_env : VariableEnvironment.t ref
  val set_variable_environment : VariableEnvironment.t -> unit
  val fresh : unit -> Variable.t
  val update_env : Expr.t -> unit

  val var : Variable.t -> Expr.t
  val zero : Type.t -> Expr.t
  val (+) : Expr.t -> Expr.t -> Expr.t
  val const : int -> Expr.t
  val ( * ) : Expr.t -> Expr.t -> Expr.t
  val case : Expr.t -> (Expr.t -> Expr.t) -> (Expr.t -> Expr.t) -> Expr.t
  val lambda : Type.t -> (Expr.t -> Expr.t) -> Expr.t
  val (@) : Expr.t -> Expr.t -> Expr.t
  val pair : Expr.t -> Expr.t -> Expr.t


  (* Helper functions for performing the symplectic form *)
end

module Val : sig
  type t =
    | Const of int
    | Pair of t * t
    | Lambda of Variable.t * Type.t * Expr.t
  val string_of_t : t -> string
  val pretty_string_of_t : t -> string
  val expr_of_t : t -> Expr.t
  val map : (int -> int) -> t -> t
  val alpha_equiv : t -> t -> bool
end


module Eval : functor (Zd : Z_SIG) -> sig
  val var_env : VariableEnvironment.t ref
  val set_variable_environment : VariableEnvironment.t -> unit

  val vzero  : Type.t -> Val.t
  val vplus  : Val.t -> Val.t -> Val.t
  val vscale : int -> Val.t -> Val.t
  val eval : Val.t VariableMap.t -> Expr.t -> Val.t

  val symplectic_form : Val.t -> Val.t -> Zd.t

end

module TypeInformation : sig
  type usage_relation = VariableSet.t -> VariableSet.t -> bool

  type ('tp, 'expr) t = {
    usage : usage_relation;
    expr : 'expr;
    tp : 'tp;
  }

  val string_of_info : ('tp -> string) -> ('expr -> string) -> ('tp,'expr) t -> string
  val pp_info : ('tp -> string) -> ('expr -> string) -> ('tp,'expr) t -> unit

  exception TypeError
  val terr : string -> 'a
  val debug : string -> unit
  val type_of_var : 'a VariableMap.t -> Variable.t -> 'a
  val assert_type : ('a -> string) -> 'a -> 'a -> unit

  val var_usage : Variable.t -> usage_relation
  val same_usage : ('tp1,'expr1) t -> ('tp2,'expr2) t -> usage_relation
  val disjoint_usage : ('tp1,'expr1) t -> ('tp2,'expr2) t -> usage_relation
  val disjoint_usage_with : ('tp1,'expr1) t -> Variable.t -> ('tp2,'expr2) t -> usage_relation
  val disjoint_usage_branch : ('tp0,'expr0) t -> Variable.t -> ('tp1,'expr1) t -> Variable.t -> ('tp2,'expr2) t -> usage_relation
end


module Typing : sig
  val string_of_info : (Type.t,Expr.t) TypeInformation.t -> string
  val pp_info : (Type.t,Expr.t) TypeInformation.t -> unit
  val assert_arrow_type : Type.t -> Type.t * Type.t
  val assert_sum_type : Type.t -> Type.t * Type.t
  val typecheck' : Type.t VariableMap.t -> Expr.t -> (Type.t,Expr.t) TypeInformation.t
  val typecheck : Expr.t -> (Type.t,Expr.t) TypeInformation.t
end
*)
