(* examples.mli *)

(** [const r] returns a LambdaC expression representing the scalar r *)
val const : int -> LambdaC.Expr.t

(** [pauli x z] returns a LambdaPC expression representing the single-qudit Pauli
    Delta(x,z)=tau^{x y}X^x Z^z
*)
val pauli : int -> int -> LambdaPC.Expr.t

(** single-qudit Z Pauli *)
val pauliZ : LambdaPC.Expr.t

(** single-qudit X Pauli *)
val pauliX : LambdaPC.Expr.t

(** single-qudit Y Pauli *)
val pauliY : LambdaPC.Expr.t

(** single-qudit identity Pauli *)
val pauliI : LambdaPC.Expr.t

(** [pauliI_ tau] returns a LambdaPC expression representing
    the identity Pauli of type [tau]
*)
val pauliI_ : LambdaC.Type.t -> LambdaPC.Expr.t

(**
  Example projective Cliffords
*)

val id : LambdaPC.Type.t -> LambdaPC.Expr.pc
val hadamard : LambdaPC.Expr.pc
val qft : LambdaPC.Expr.pc
val phasegate : LambdaPC.Expr.pc

(* Length-indexed Pauli types *)

exception IllFormedType

(** [ntensor n] returns the type [Pauli ** (Pauli ** ... )] *)
val ntensor : int -> LambdaPC.Type.t
val in_n_i : int -> int -> LambdaPC.Expr.t -> LambdaPC.Expr.t
val match_in_i :
  int ->
  LambdaPC.Expr.t -> (int -> LambdaPC.Expr.t -> LambdaPC.Expr.t) -> LambdaPC.Expr.t
val ptensor :
  LambdaPC.Type.t ->
  LambdaPC.Type.t -> LambdaPC.Expr.t -> LambdaPC.Expr.t -> LambdaPC.Expr.t
val pauliNegX2Y3 : LambdaPC.Expr.t
val pauliXY : LambdaPC.Expr.t
val swap : LambdaPC.Type.t -> LambdaPC.Type.t -> LambdaPC.Expr.pc
val cnot : LambdaPC.Expr.pc
module S2 : Scalars.SCALARS
module Eval2 :
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
          LambdaC.Val.t LambdaC.VariableMap.t ->
          LambdaC.Expr.t -> LambdaC.Val.t
        val symplectic_form : LambdaC.Val.t -> LambdaC.Val.t -> S2.Zd.t
      end
    module LEval' :
      sig
        val var_env : VarEnv.t ref
        val set_variable_environment : VarEnv.t -> unit
        val vzero : LambdaC.Type.t -> LambdaC.Val.t
        val vplus : LambdaC.Val.t -> LambdaC.Val.t -> LambdaC.Val.t
        val vscale : int -> LambdaC.Val.t -> LambdaC.Val.t
        val eval :
          LambdaC.Val.t LambdaC.VariableMap.t ->
          LambdaC.Expr.t -> LambdaC.Val.t
        val symplectic_form : LambdaC.Val.t -> LambdaC.Val.t -> S2.Zd'.t
      end
    val sgn : LambdaC.Val.t -> S2.Zd0.t
    val add_phase : int -> LambdaPC.Val.t -> LambdaPC.Val.t
    val cprod_phase : LambdaC.Val.t -> LambdaC.Val.t -> S2.Zd.t
    val cfg_prod : LambdaPC.Val.t -> LambdaPC.Val.t -> LambdaPC.Val.t
    val pow_phase : LambdaC.Val.t -> int -> S2.Zd.t
    val cfg_pow : LambdaPC.Val.t -> int -> LambdaPC.Val.t
    val case_phase : int -> int -> int
    val eval :
      LambdaC.Val.t LambdaC.VariableMap.t ->
      LambdaPC.Expr.t -> LambdaPC.Val.t
    val evalClosed : LambdaPC.Expr.t -> LambdaPC.Val.t
  end
val evalTest : unit -> unit
