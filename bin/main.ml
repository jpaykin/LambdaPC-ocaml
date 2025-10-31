
open PCLib

(*
module Z2 = Qudits.Z(Qudits.FIN2)
module E = LambdaC.Expr(Z2)
module V = LambdaC.Val(Z2)
module Eval = LambdaC.Eval(Z2)


let lexpr_example_1 : E.t = Plus (Var 0, Const (Z2.t_of_int 3))
let lexpr_example_2 : E.t = E.Lambda (1, Unit, Plus (Var 1, Zero Unit))
let lexpr_example_3 : E.t = E.Apply (lexpr_example_2, lexpr_example_1)

let ctx_example = LambdaC.VariableMap.singleton 0 (V.Const (Z2.t_of_int 39))
let env_example : LambdaC.environment = { fresh_var = ref 100 }
let lval_example = Eval.eval env_example ctx_example lexpr_example_3


(** Tests *)
let () = print_endline (V.string_of_t lval_example)
*)

let () = Examples.evalTest()