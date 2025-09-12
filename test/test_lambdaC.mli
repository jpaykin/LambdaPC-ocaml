open PCLib
open Scalars
open LambdaC

module TestEval (_ : Z_SIG) : sig

  val test_val_eq : Val.t -> Val.t -> Alcotest.return
  val test_eval : Expr.t -> Expr.t -> Alcotest.return

end

val suite : (string * Alcotest.return Alcotest.test_case list) list
