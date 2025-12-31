open LambdaPC

module LinearityTyping : sig
  type type_information = (LambdaPC.Type.t, LambdaPC.Expr.t) LambdaC.TypeInformation.t

  val string_of_info : type_information -> string
  val pp_info : type_information -> unit

  val typecheck' : Type.t VariableMap.t -> Expr.t -> type_information
  val linearity_check : Expr.t -> type_information
  val linearity_check_pc : Expr.pc -> (LambdaPC.Type.t * LambdaPC.Type.t, LambdaPC.Expr.pc) LambdaC.TypeInformation.t
end

module SmtLambdaCExpr : sig
  open LambdaC
    type normal =
      NConst of int
    | NLambda of Variable.t * Type.t * normal
    | NPair of normal * normal
    | Annot of neutral * Type.t
    | Neutral of neutral
    and neutral =
      NVar of Variable.t
    | NApply of neutral * normal
    | NCase of neutral * Variable.t * normal * Variable.t * normal
    | NPlus of neutral * normal
    | NScale of normal * neutral

  (* Normalized expressions do not contain Zero or
     Let statements.
     Normalized expressions preserve types but NOT linearity checking.
     If an arrow type -o does not occur in the free variables of an expression or in its result type,
     then its normalization should not contain any instances of Lambda or Apply
  *)
  val normalize : LambdaC.Expr.t -> LambdaC.Expr.t
end

module SmtLambdaC :
  (_ : Scalars.Z_SIG) -> sig
  open LambdaC
    val smtml_of_type : LambdaC.Type.t -> Smtml.Ty.t
    val smtml_of_expr : Type.t VariableMap.t -> Smtml.Symbol.t VariableMap.t -> Expr.t -> Type.t -> Smtml.Expr.t
    val value_of_smtml : Type.t -> Smtml.Value.t -> Val.t

  type counterexample = {
    inputs : Val.t VariableMap.t;
    lhs : Val.t;
    rhs : Val.t
  }

    val equiv : Type.t -> Type.t VariableMap.t -> Expr.t -> Expr.t -> (unit, counterexample) result
  end

module SmtLambdaPC :
  (_ : Scalars.SCALARS) -> sig
  val symplectic_check : Type.t -> Type.t -> LambdaPC.Expr.pc -> unit
  val typecheck : LambdaPC.Expr.pc -> Type.t * Type.t
end