open Alcotest

let () =
  (*let () = Test_scalars.tests() in*)
  run "LambdaPC tests" (
    Test_scalars.suite
    @
    Test_lambdaC.suite
    @
    Test_lambdaPC.suite;
  )