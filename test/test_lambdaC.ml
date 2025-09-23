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
    let ctx = VariableMap.empty in
    let v1 = EvalZd.eval ctx e1 in
    let v2 = EvalZd.eval ctx e2 in
    test_val_eq v1 v2
end


module Z2 = Z(FIN2)
module Eval2 = Eval(Z2)
module TestEval2 = TestEval(Z2)

(* Additional unit tests for individual functions in LambdaC *)
let test_expr_rename_var () =
  let e1_test = Expr.(Lambda (1, Unit, Plus (Var 1, Var 2))) in
  let e1' = Expr.rename_var 2 42 e1_test in
  let e1_expected = Expr.Lambda(1, Unit, Plus (Var 1, Var 42)) in
  (* ensure only free occurrences renamed, bound var 1 unchanged *)
  check string "rename_var_free" (Expr.string_of_t e1') (Expr.string_of_t e1_expected);

  let e2' = Expr.rename_var 1 42 e1_test in
  check string "rename_var_bound" (Expr.string_of_t e2') (Expr.string_of_t e1_test)

let test_expr_map_and_val_map () =
  let e1 = Expr.(Plus (Const 3, Scale (Const 2, Var 5))) in
  let e1_mapped = Expr.map (fun x -> x + 1) e1 in
  let e1_expected = Expr.(Plus (Const 4, Scale (Const 3, Var 5))) in
  check string "expr_map" (Expr.string_of_t e1_mapped) (Expr.string_of_t e1_expected);

  let v1 = Val.(Pair (Const 3, Val.Lambda (7, Unit, Const 2))) in
  let v1_mapped = Val.map (fun x -> x * 2) v1 in
  let v1_expected = Val.(Pair (Const 6, Lambda (7, Unit, Const 4))) in
  check string "val_map" (Val.string_of_t v1_mapped) (Val.string_of_t v1_expected)

let test_vzero_vplus_vscale_case_apply () =
  let open Eval2 in
  (* vzero Unit = Const 0 *)
  check string "vzero Unit" (Val.string_of_t (vzero Unit)) ("0");
  check string "vzero Pair" (Val.string_of_t (vzero (Sum (Unit, Arrow(Unit,Unit)))))
              Val.(string_of_t (Pair(Const 0, Lambda(3, Unit, Expr.Const 0))));

  (* vplus on consts *)
  let plus_res = vplus (Val.Const 1) (Val.Const 1) in
  check string "vplus consts" (Val.string_of_t plus_res) ("0") ;
  (* Note: in Z2 arithmetic 1+1 = 0 mod 2 *)

  (* vscale on pair *)
  let scaled = vscale 3 (Val.Pair (Val.Const 1, Val.Const 0)) in
  check string "vscale pair" (Val.string_of_t scaled) ("(3, 0)")


let test_symplectic_form () =
  let open Eval2 in
  let v1 = Val.Pair (Val.Const 1, Val.Const 0) in
  let v2 = Val.Pair (Val.Const 0, Val.Const 1) in
  let z = symplectic_form v1 v2 in
  (* For Z2, 1*1 - 0*0 = 1 *)
  check int "symplectic_form" (Z2.int_of_t z) 1

let suite =
  [ "TestEvalZ2", [
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

      test_case "Testing case evaluation" `Quick (fun () ->
        TestEval2.test_eval
          HOAS.(case (Expr.Pair(const 1, const 0))
            (fun x1 -> var x1)
            (fun _ -> const 11)
          )
          HOAS.(const 12)
        );

      test_case "Expr.rename_var" `Quick test_expr_rename_var;
      test_case "Expr.map and Val.map" `Quick test_expr_map_and_val_map;
      test_case "vzero/vplus/vscale/Case/Apply" `Quick test_vzero_vplus_vscale_case_apply;
      test_case "symplectic_form" `Quick test_symplectic_form;
    ];
  ]