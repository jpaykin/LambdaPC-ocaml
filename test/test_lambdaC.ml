open Alcotest
open PCLib
open Scalars
open LambdaC
open Test_scalars

module TestEval (Zd : Z_SIG) = struct
  module TestZd = TestZ(Zd)
  module EvalZd = Eval(Zd)
  let rec test_val_eq v1 v2 =
    match v1, v2 with
    | Val.Const r1, Val.Const r2 -> TestZd.test_eq_of_int r1 r2
    | Val.Pair (v11, v12), Val.Pair (v21, v22) ->
      test_val_eq v11 v21;
      test_val_eq v12 v22
    | Val.Lambda _, Val.Lambda _ ->
      failf "Could not verify two lambdas are equal.\n  1: %s \n  2: %s\n"
        (Val.string_of_t v1) (Val.string_of_t v2)
    | _, _ ->
      failf "Values not equal.\n  1: %s \n  2: %s\n" (Val.string_of_t v1) (Val.string_of_t v2)

  let test_eval e1 e2 =
    let env : environment = { fresh_var = ref 0; } in
    let ctx = VariableMap.empty in
    let v1 = EvalZd.eval env ctx e1 in
    let v2 = EvalZd.eval env ctx e2 in
    test_val_eq v1 v2
end


module Z2 = Z(FIN2)
module Eval2 = Eval(Z2)
module TestEval2 = TestEval(Z2)

let suite =
  ["TestEvalZ2", [
      test_case "Testing 5+3 = 8"   `Quick (fun () -> 
        TestEval2.test_eval
          HOAS.(const 5 + const 3)
          HOAS.(const 8)
        );

      test_case "Testing function application" `Quick (fun () ->
        TestEval2.test_eval
          HOAS.(lambda Unit (fun x -> var x + var x) @ (const 3))
          HOAS.(const 6)
        );
    ];
  ]
