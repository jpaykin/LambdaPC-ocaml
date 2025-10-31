open LambdaPC.HOAS

let const = LambdaC.HOAS.const
let pauli r1 r2 = vec (LambdaC.Expr.Pair (const r1, const r2))

let pauliZ : LambdaPC.Expr.t = pauli 0 1
let pauliX : LambdaPC.Expr.t = pauli 1 0
let pauliY : LambdaPC.Expr.t = pauli 1 1
let pauliI : LambdaPC.Expr.t = pauli 0 0

let pauliNegX2Y3 = phase (const 1) (pauliI ** pauliX ** pauliY ** pauliI)

let id tp = lambda tp (fun q -> var q)
let hadamard = lambda Pauli (fun q -> case1 (var q) pauliZ pauliX)


(* Testing *)
(*Scalars (Z FIN2)*)
module S2 = Scalars.Scalars (Scalars.FIN2)
module Eval2 = LambdaPC.Eval(S2)
(* Check that hadamard Y = -Y *)

let printValuation e =
  let result = Eval2.evalClosed e in
  print_endline (LambdaPC.Expr.pretty_string_of_t e ^ "\n->*\n" ^ LambdaPC.Val.string_of_t result ^ "\n")

let evalTest () =

  print_endline ("-1 = " ^ S2.Zd.string_of_t (S2.Zd.t_of_int (-1)));
  let x = Eval2.cprod_phase (LambdaC.Val.Pair (LambdaC.Val.Const 1, LambdaC.Val.Const 0)) (LambdaC.Val.Pair (LambdaC.Val.Const 0, LambdaC.Val.Const 1)) in
  print_endline ("got " ^ S2.Zd.string_of_t x);

  print_endline "\n";
  printValuation (pauliY);
  printValuation (pow pauliX 1);
  printValuation (pow pauliZ 0);
  printValuation (hadamard @ pauliX);
  printValuation (hadamard @ (phase (const 1) pauliZ));
  printValuation (pauliZ ** pauliX);
  printValuation (pauliX ** pauliZ);
  printValuation (pauliX ** pauliX);
  printValuation (hadamard @ pauliY);
  print_endline "\n"