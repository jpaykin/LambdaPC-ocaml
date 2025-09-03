open Scalars

type ltype = Unit | Sum of ltype * ltype | Arrow of ltype * ltype
val string_of_ltype : ltype -> string

module Variable : Map.OrderedType with type t = int
module VariableMap : Map.S with type key = Variable.t
type environment = { fresh_var : Variable.t ref }
val fresh : environment -> Variable.t

module Expr : sig
  type t =
      Var of Variable.t
    | Zero of ltype
    | Plus of t * t
    | Const of int
    | Scale of t * t
    | Pair of t * t
    | Case of t * Variable.t * t * Variable.t * t
    | Lambda of Variable.t * ltype * t
    | Apply of t * t

  val string_of_t : t -> string
  val rename_var : Variable.t -> Variable.t -> t -> t
  val map : (int -> int) -> t -> t
end


module HOAS : sig
  val env : environment
  val var : Variable.t -> Expr.t
  val zero : ltype -> Expr.t
  val (+) : Expr.t -> Expr.t -> Expr.t
  val const : int -> Expr.t
  val ( * ) : Expr.t -> Expr.t -> Expr.t
  val case : Expr.t -> (Variable.t -> Expr.t) -> (Variable.t -> Expr.t) -> Expr.t
  val lambda : ltype -> (Variable.t -> Expr.t) -> Expr.t
  val (@) : Expr.t -> Expr.t -> Expr.t    
end

module Val : sig
  type t =
    | Const of int
    | Pair of t * t
    | Lambda of Variable.t * ltype * Expr.t
  val string_of_t : t -> string
  val expr_of_t : t -> Expr.t
  val map : (int -> int) -> t -> t
end


module Eval : functor (Zd : Z_SIG) -> sig

  val vzero  : ltype -> environment -> Val.t
  val vplus  : environment -> Val.t -> Val.t -> Val.t
  val vscale : environment -> int -> Val.t -> Val.t
  val eval : environment -> Val.t VariableMap.t -> Expr.t -> Val.t

  val symplectic_form : Val.t -> Val.t -> Zd.t

end


(* module Expr_Functor (A : Z_SIG) (B : Z_SIG) : sig
  val map_expr : (A.t -> B.t) -> Expr.t -> Expr.t
  val map_lval : (A.t -> B.t) -> Val.t -> Val.t
end *)



module Conversions (Conv : SCALARS) : sig
  open Conv
  val sgn : Val.t -> Zd0.t
end