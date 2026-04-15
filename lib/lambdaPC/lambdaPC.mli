open Ident

module Type : sig
    type t = { loc : Loc.t option; node : node }
    and node =
      | Pauli
      | PTensor of t * t
    
    
    val pauli : t
    val ( ** ) : t -> t -> t

    val t_of_node : node -> t
    val ltype_of_t : t -> LambdaC.Type.t
    val t_of_ltype : LambdaC.Type.t -> t option
    val string_of_t : t -> string
end
module Expr :
  sig
    type t = { loc : Loc.t option; ty : Type.t option; node : node }
    and node =
      | Var of Ident.t
      | Let of { x : Ident.t; expr : t; body : t }
      | LExpr of LambdaC.Expr.t
      | Phase of LambdaC.Expr.t * t
      | Prod of t * t
      | Pow of t * LambdaC.Expr.t
      | CasePauli of { scrut : t; tx : t; tz : t }
      | In1 of { tp : Type.t; v : t }
      | In2 of { tp : Type.t; v : t }
      | CasePTensor of { scrut : t; x1 : Ident.t; t1 : t; x2 : Ident.t; t2 : t }
      | App of pc * t
      | Force of p
    
    and pc = { loc : Loc.t option; ty : (Type.t * Type.t) option; node : pc_node }
    and pc_node = Lam of { x : Ident.t; tp : Type.t; body : t }
    
    and p = { loc : Loc.t option; ty : Type.t option; node : p_node }
    and p_node = Suspend of t

    val t_of_node : node -> t
    val pc_of_node : pc_node -> pc
    val p_of_node : p_node -> p
    
    val string_of_t : t -> string
    val string_of_pc : pc -> string
    val string_of_p : p -> string
    val pretty_string_of_t : t -> string
    val pretty_string_of_pc : pc -> string
    val pretty_string_of_p : p -> string

    val rename_var : Ident.t -> Ident.t -> t -> t
  end
module Val :
  sig
    type t = { phase : int; value : LambdaC.Val.t; }
    val string_of_t : t -> string
    val pure : LambdaC.Val.t -> t
  end

module HOAS : sig

  val var : Ident.t -> Expr.t
  val letin : Expr.t -> (Expr.t -> Expr.t) -> Expr.t
  val vec : LambdaC.Expr.t -> Expr.t
  val phase : LambdaC.Expr.t -> Expr.t -> Expr.t
  val ( * ) : Expr.t -> Expr.t -> Expr.t
  val pow : Expr.t -> LambdaC.Expr.t -> Expr.t
  val caseofP : Expr.t -> Expr.t -> Expr.t -> Expr.t
  val in1 : Expr.t -> Type.t -> Expr.t
  val in2 : Type.t -> Expr.t -> Expr.t
  val caseof : Expr.t -> (Expr.t -> Expr.t) -> (Expr.t -> Expr.t) -> Expr.t
  val lambda : Type.t -> (Expr.t -> Expr.t) -> Expr.pc
  val (@) : Expr.pc -> Expr.t -> Expr.t
  val suspend : Expr.t -> Expr.p
  val force : Expr.p -> Expr.t
end

module SymplecticForm : sig
  val psi_of : Expr.t -> LambdaC.Expr.t
  val psi_of_pc : Expr.pc -> LambdaC.Expr.t
  val ccaseP : LambdaC.Expr.t -> LambdaC.Expr.t -> LambdaC.Expr.t -> LambdaC.Expr.t
  val omega : Type.t -> LambdaC.Expr.t -> LambdaC.Expr.t -> LambdaC.Expr.t 
end

module PhaseEnvironment : functor (Zd : Scalars.Z_SIG) ->
    sig
      type t = Zd.t ref
      val init : t
      val add_phase : t -> Zd.t -> unit
      val add_integer_phase : t -> int -> unit
    end
module Eval : functor (S : Scalars.SCALARS) ->
    sig
      module LEval :
        sig
          val vzero : LambdaC.Type.t -> LambdaC.Val.t
          val vplus : LambdaC.Val.t -> LambdaC.Val.t -> LambdaC.Val.t
          val vscale : int -> LambdaC.Val.t -> LambdaC.Val.t
          val eval :
            LambdaC.Val.t VariableMap.t -> LambdaC.Expr.t -> LambdaC.Val.t
          val symplectic_form : LambdaC.Val.t -> LambdaC.Val.t -> S.Zd.t
        end
      module LEval' :
        sig
          val vzero : LambdaC.Type.t -> LambdaC.Val.t
          val vplus : LambdaC.Val.t -> LambdaC.Val.t -> LambdaC.Val.t
          val vscale : int -> LambdaC.Val.t -> LambdaC.Val.t
          val eval :
            LambdaC.Val.t VariableMap.t -> LambdaC.Expr.t -> LambdaC.Val.t
          val symplectic_form : LambdaC.Val.t -> LambdaC.Val.t -> S.Zd'.t
        end
      val sgn : LambdaC.Val.t -> S.Zd0.t
      val add_phase : int -> Val.t -> Val.t
      val cprod_phase : LambdaC.Val.t -> LambdaC.Val.t -> S.Zd.t
      val cfg_prod : Val.t -> Val.t -> Val.t
      val pow_phase : LambdaC.Val.t -> int -> S.Zd.t
      val cfg_pow : Val.t -> int -> Val.t
      val case_phase : int -> int -> int
      val eval : LambdaC.Val.t VariableMap.t -> Expr.t -> Val.t

      val evalClosed : Expr.t -> Val.t
    end
