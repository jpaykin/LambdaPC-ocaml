open Scalars
open Ident

module Type = Ast.Type
module Expr = Ast.Expr

module HOAS : sig

  val var : Ident.t -> Expr.t
  val zero : ?ty:Type.t -> unit -> Expr.t
  val (+) : Expr.t -> Expr.t -> Expr.t
  val const : int -> Expr.t
  val ( * ) : Expr.t -> Expr.t -> Expr.t
  val case : Expr.t -> (Expr.t -> Expr.t) -> (Expr.t -> Expr.t) -> Expr.t
  val lambda : Type.t -> (Expr.t -> Expr.t) -> Expr.t
  val (@) : Expr.t -> Expr.t -> Expr.t
  val pair : Expr.t -> Expr.t -> Expr.t
  val u : Type.t
  val (++) : Type.t -> Type.t -> Type.t
  val lolli : Type.t -> Type.t -> Type.t

end

module Val : sig
  type t =
    | Const of int
    | Pair of t * t
    | Lambda of Ident.t * Type.t * Expr.t
  val string_of_t : t -> string
  val pretty_string_of_t : t -> string
  val expr_of_t : t -> Expr.t
  val map : (int -> int) -> t -> t
  val alpha_equiv : t -> t -> bool
end


module Eval : functor (Zd : Z_SIG) -> sig

  val vzero  : Type.t -> Val.t
  val vplus  : Val.t -> Val.t -> Val.t
  val vscale : int -> Val.t -> Val.t
  val eval : Val.t VariableMap.t -> Expr.t -> Val.t

  val symplectic_form : Val.t -> Val.t -> Zd.t

end

module TypeInformation : sig
  type usage_relation = IdentSet.t -> IdentSet.t -> bool

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
  val type_of_var : 'a VariableMap.t -> Ident.t -> 'a
  val assert_type : ('a -> string) -> 'a -> 'a -> unit

  val var_usage : Ident.t -> usage_relation
  val same_usage : ('tp1,'expr1) t -> ('tp2,'expr2) t -> usage_relation
  val disjoint_usage : ('tp1,'expr1) t -> ('tp2,'expr2) t -> usage_relation
  val disjoint_usage_with : ('tp1,'expr1) t -> Ident.t -> ('tp2,'expr2) t -> usage_relation
  val disjoint_usage_branch : ('tp0,'expr0) t -> Ident.t -> ('tp1,'expr1) t -> Ident.t -> ('tp2,'expr2) t -> usage_relation
end


module Typing : sig
  val string_of_info : (Type.t,Expr.t) TypeInformation.t -> string
  val pp_info : (Type.t,Expr.t) TypeInformation.t -> unit
  val assert_arrow_type : Type.t -> Type.t * Type.t
  val assert_sum_type : Type.t -> Type.t * Type.t
  val annot : Expr.t -> Type.t -> Expr.t
  val typecheck' : Type.t VariableMap.t -> Expr.t -> (Type.t,Expr.t) TypeInformation.t
  val typecheck : Expr.t -> (Type.t,Expr.t) TypeInformation.t
end
