open Alcotest
open PCLib
open Scalars
(*open LambdaC
open LambdaPC
*)
module TestEval (Zd : SCALARS) = struct
  module EvalZd = LambdaPC.Eval(Zd)
end

let suite =
  ["TestPCZ2", [
    test_case "Testing 1+1=2" `Quick (fun () -> check int "addition" (1+1) 2)
    ];
  ]