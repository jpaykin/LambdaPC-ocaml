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

let suite =
  ["TestPCZ2", [
    test_case "Testing 1+1=2" `Quick (fun () -> check int "addition" (1+1) 2);
    test_case "Parser accepts zero{tau}" `Quick test_parse_zero_surface;
    test_case "Parser accepts braced injections" `Quick test_parse_braced_injections;
    ];
  ] 
