open Alcotest
open PCLib

module TestZ (D : Scalars.FIN) = struct
  module ZD = Scalars.Z(D)

  let check_eq (name : string) (x : ZD.t) (y : ZD.t) =
    check int name (ZD.int_of_t x) (ZD.int_of_t y)
  let check_neq (name : string) (x : ZD.t) (y : ZD.t) =
    check (neg int) name (ZD.int_of_t x) (ZD.int_of_t y)
  let test_eq_of_int (x : int) (y : int) =
    check_eq "test_eq_of_int" (ZD.t_of_int(x)) (ZD.t_of_int(y))
  let test_neq_of_int (x : int) (y : int) =
    check_neq "test_neq_of_int" (ZD.t_of_int(x)) (ZD.t_of_int(y))
end

module TestZ2 = TestZ(Scalars.FIN2)
module TestZ3 = TestZ(Scalars.FIN3)

let () =
  run "Qudits Test" [
    "TestZ2", [
      test_case "Testing 3=5"   `Quick (fun () -> TestZ2.test_eq_of_int 3 5);
      test_case "Testing 2=20"  `Quick (fun () -> TestZ2.test_eq_of_int 2 20);
      test_case "Testing 2<>15" `Quick (fun () -> TestZ2.test_neq_of_int 2 15);
    ];

    "TestZ3", [
      test_case "Testing 3=0"   `Quick (fun () -> TestZ3.test_eq_of_int 3 0);
      test_case "Testing 2=14"  `Quick (fun () -> TestZ3.test_eq_of_int 2 14);
      test_case "Testing 3<>20" `Quick (fun () -> TestZ3.test_neq_of_int 3 20);
      test_case "Testing 4<>20" `Quick (fun () -> TestZ3.test_neq_of_int 4 20);
    ];
  ]