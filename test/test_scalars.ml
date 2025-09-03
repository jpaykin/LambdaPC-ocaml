open Alcotest
open PCLib
open Scalars

module TestZ (ZD : Z_SIG) = struct

  let check_eq (name : string) (x : ZD.t) (y : ZD.t) =
    check int name (ZD.int_of_t x) (ZD.int_of_t y)
  let check_neq (name : string) (x : ZD.t) (y : ZD.t) =
    check (neg int) name (ZD.int_of_t x) (ZD.int_of_t y)
  let test_eq_of_int (x : int) (y : int) =
    check_eq "test_eq_of_int" (ZD.t_of_int(x)) (ZD.t_of_int(y))
  let test_neq_of_int (x : int) (y : int) =
    check_neq "test_neq_of_int" (ZD.t_of_int(x)) (ZD.t_of_int(y))

end

module TestConversions (S : SCALARS) = struct
  module TestZd = TestZ(S.Zd)
  module TestZd' = TestZ(S.Zd')
  module TestZd0 = TestZ(S.Zd0)
  
  let test_inc_d_d' (x : int) =
    let d = S.Zd.Dim.dim in
    let x_d = S.Zd.t_of_int(x) in
    let x_d' = S.Zd'.t_of_int(x) in
    if S.Zd'.normalize(x) < d
    then TestZd'.check_eq "test_inc_d_d" (S.inc_d_d' x_d) x_d'
    else TestZd'.check_eq "test_inc_d_d" (S.inc_d_d' x_d) S.Zd'.(x_d' - t_of_int(d))

  (* Check that (x mod d = x) mod d *)
  let test_mod_d'_d (x : int) =
    let x_d = S.Zd.t_of_int(x) in
    let x_d' = S.Zd'.t_of_int(x) in
    TestZd.check_eq "test_mod_d'_d" (S.mod_d'_d x_d') x_d


  let test_sgn (x : int) =
    let d = S.Zd.Dim.dim in
    let x_norm = S.Zd'.normalize(x) in
    let x_d' = S.Zd'.t_of_int(x) in
    if x_norm < d
      then TestZd0.check_eq "test_sgn" (S.sgn x_d') (S.Zd0.t_of_int(0))
      else TestZd0.check_eq "test_sgn" (S.sgn x_d') (S.Zd0.t_of_int(1))
end

module Z2 = Z(FIN2)
module Z3 = Z(FIN3)
module TestZ2 = TestZ(Z2)
module TestZ3 = TestZ(Z3)
module Conversions2 = Scalars(FIN2)
module TestConversions2 = TestConversions(Conversions2)

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

    "TestConversions2", [
      test_case "Testing inc_d_d' for d=2" `Quick
          (fun () -> TestConversions2.test_inc_d_d' 3);
      test_case "Testing inc_d_d' for d=2" `Quick
          (fun () -> TestConversions2.test_inc_d_d' 0);

      test_case "Testing mod_d'_d for d=2" `Quick
          (fun () -> TestConversions2.test_mod_d'_d 3);
      test_case "Testing mod_d'_d for d=2" `Quick
          (fun () -> TestConversions2.test_mod_d'_d 0);

      test_case "Testing sgn for d=2" `Quick
          (fun () -> TestConversions2.test_sgn 0);
      test_case "Testing sgn for d=2" `Quick
          (fun () -> TestConversions2.test_sgn 1);
      test_case "Testing sgn for d=2" `Quick
          (fun () -> TestConversions2.test_sgn 2);
      test_case "Testing sgn for d=2" `Quick
          (fun () -> TestConversions2.test_sgn 3);
    ]
  ]