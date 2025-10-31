module Type : sig
    type t = Pauli | PTensor of t*t
    val ltype_of_t : t -> LambdaC.Type.t
    val string_of_t : t -> string
end
module Variable = LambdaC.Variable
module VariableMap = LambdaC.VariableMap
module Expr :
  sig
    type t =
        Var of Variable.t
      | Let of t * Variable.t * t
      | LExpr of LambdaC.Expr.t
      | Phase of LambdaC.Expr.t * t
      | Prod of t * t
      | Pow of t * int
      | CasePauli of t * t * t
      | In1 of t * Type.t
      | In2 of Type.t * t
      | CasePTensor of t * Variable.t * t * Variable.t * t
      | Apply of pc * t
      | Force of p
    and pc = Lam of (Variable.t * Type.t * t)
    and p = Suspend of t

    val string_of_t : t -> string
    val string_of_pc : pc -> string
    val string_of_p : p -> string
    val rename_var : int -> int -> t -> t
  end
module Val :
  sig
    type t = { phase : int; value : LambdaC.Val.t; }
    val string_of_t : t -> string
    val pure : LambdaC.Val.t -> t
  end

module HOAS : sig
  val fresh : unit -> Variable.t

  val var : Variable.t -> Expr.t
  val letin : Expr.t -> (Variable.t -> Expr.t) -> Expr.t
  val vec : LambdaC.Expr.t -> Expr.t
  val phase : LambdaC.Expr.t -> Expr.t -> Expr.t
  val ( ** ) : Expr.t -> Expr.t -> Expr.t
  val pow : Expr.t -> int -> Expr.t
  val case1 : Expr.t -> Expr.t -> Expr.t -> Expr.t
  val in1 : Expr.t -> Type.t -> Expr.t
  val in2 : Type.t -> Expr.t -> Expr.t
  val case : Expr.t -> (Variable.t -> Expr.t) -> (Variable.t -> Expr.t) -> Expr.t
  val lambda : Type.t -> (Variable.t -> Expr.t) -> Expr.pc
  val (@) : Expr.pc -> Expr.t -> Expr.t
  val suspend : Expr.t -> Expr.p
  val force : Expr.p -> Expr.t
end

module PhaseEnvironment : (Zd : Scalars.Z_SIG) ->
    sig
      type t = Zd.t ref
      val init : t
      val add_phase : t -> Zd.t -> unit
      val add_integer_phase : t -> int -> unit
    end
module Eval :
  (S : Scalars.SCALARS) ->
    sig
      module VarEnv = LambdaC.VariableEnvironment
      val var_env : VarEnv.t ref
      val set_variable_environment : VarEnv.t -> unit
      val fresh : unit -> int
      module LEval :
        sig
          val var_env : VarEnv.t ref
          val set_variable_environment : VarEnv.t -> unit
          val vzero : LambdaC.Type.t -> LambdaC.Val.t
          val vplus : LambdaC.Val.t -> LambdaC.Val.t -> LambdaC.Val.t
          val vscale : int -> LambdaC.Val.t -> LambdaC.Val.t
          val eval :
            LambdaC.Val.t VariableMap.t -> LambdaC.Expr.t -> LambdaC.Val.t
          val symplectic_form : LambdaC.Val.t -> LambdaC.Val.t -> S.Zd.t
        end
      module LEval' :
        sig
          val var_env : VarEnv.t ref
          val set_variable_environment : VarEnv.t -> unit
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
    end
