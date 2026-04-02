open Alcotest
open PCLib
open Scalars
open Named_ast
(*open LambdaC
open LambdaPC
*)
module TestEval (Zd : SCALARS) = struct
  module EvalZd = LambdaPC.Eval(Zd)
end

let test_parse_zero_surface () =
  match Interface.parse "zero{Zd}" with
  | { LambdaPC_Surface.node =
        LExpr
          { LambdaC_Surface.node =
              Zero { LambdaC_Surface.Type.node = LambdaC_Surface.Type.Unit; _ }
          ; _
          }
    ; _ } -> ()
  | ast ->
      failf "Expected zero surface term, got %s\n"
        (LambdaPC_Surface.pretty_string_of_expr ast)

let test_parse_braced_injections () =
  let check_in1 () =
    match Interface.parse "in1{Pauli} X" with
    | { LambdaPC_Surface.node =
          In1
            { tp = { LambdaPC_Surface.Type.node = LambdaPC_Surface.Type.Pauli; _ }
            ; v = { LambdaPC_Surface.node = LExpr _; _ }
            }
      ; _ } -> ()
    | ast ->
        failf "Expected braced in1 term, got %s\n"
          (LambdaPC_Surface.pretty_string_of_expr ast)
  in
  let check_in2 () =
    match Interface.parse "in2{Pauli} X" with
    | { LambdaPC_Surface.node =
          In2
            { tp = { LambdaPC_Surface.Type.node = LambdaPC_Surface.Type.Pauli; _ }
            ; v = { LambdaPC_Surface.node = LExpr _; _ }
            }
      ; _ } -> ()
    | ast ->
        failf "Expected braced in2 term, got %s\n"
          (LambdaPC_Surface.pretty_string_of_expr ast)
  in
  check_in1 ();
  check_in2 ()

let test_parse_reports_resolve_errors () =
  match Interface.parse "let x = X in y" with
  | _ -> fail "Expected unresolved identifier to raise Parse_error"
  | exception Interface.Parse_error (_loc, msg) ->
      check bool "resolve error mentions unbound variable"
        true (String.starts_with ~prefix:"unbound LambdaPC variable" msg)

let test_named_ast_seed_fresh_expr () =
  match Interface.parse "let x = X in x" with
  | { LambdaPC_Surface.node = Let { x; _ }; _ } ->
      Named_ast.LambdaPC_Surface.seed_fresh
        (Interface.parse "let x = X in x");
      let fresh = Fresh.fresh ~hint:"after_surface" () in
      check bool "fresh is above resolved surface binder" true (fresh > x.sym)
  | ast ->
      failf "Expected let-bound surface term, got %s\n"
        (LambdaPC_Surface.pretty_string_of_expr ast)

let test_lambdapc_update_env_seeds_fresh () =
  let env = LambdaC.VariableEnvironment.create () in
  let pc = LambdaPC.Expr.Lam (9000, LambdaPC.Type.Pauli, LambdaPC.Expr.Var 9000) in
  LambdaPC.Expr.update_env_pc env pc;
  let x = LambdaPC.HOAS.fresh () in
  check bool "fresh is above LambdaPC binder" true (x > 9000)

let suite =
  ["TestPCZ2", [
    test_case "Testing 1+1=2" `Quick (fun () -> check int "addition" (1+1) 2);
    test_case "Parser accepts zero{tau}" `Quick test_parse_zero_surface;
    test_case "Parser accepts braced injections" `Quick test_parse_braced_injections;
    test_case "Parser reports resolve errors" `Quick test_parse_reports_resolve_errors;
    test_case "Named_ast seed_fresh seeds surface binders" `Quick test_named_ast_seed_fresh_expr;
    test_case "LambdaPC update_env seeds freshness" `Quick test_lambdapc_update_env_seeds_fresh;
    ];
  ] 
