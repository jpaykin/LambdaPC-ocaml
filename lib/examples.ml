open LambdaPC.HOAS

let const = LambdaC.HOAS.const
let pauli r1 r2 = vec (LambdaC.Expr.Pair (const r1, const r2))

let pauliZ : LambdaPC.Expr.t = pauli 0 1
let pauliX : LambdaPC.Expr.t = pauli 1 0
let pauliY : LambdaPC.Expr.t = pauli 1 1
let pauliI : LambdaPC.Expr.t = pauli 0 0

let pauliI_ tp = vec (LambdaC.HOAS.zero tp)

let id tp = lambda tp (fun q -> var q)
let hadamard = lambda Pauli (fun q -> caseofP (var q) pauliZ pauliX)
let qft = lambda Pauli (fun q -> caseofP (var q) pauliZ (pow pauliX (-1)))
let phase = lambda Pauli (fun q ->
    caseofP (var q)
      (*X->*) pauliY
      (*Z->*) pauliZ
  )


exception IllFormedType

let rec ntensor (n : int) : LambdaPC.Type.t =
  assert (n >= 1);
  if n = 1 then Pauli
  else PTensor (Pauli, ntensor (n-1))

let rec in_n_i (n : int) (i : int) (t : LambdaPC.Expr.t) : LambdaPC.Expr.t =
  assert (n >= 1 && 0 <= i && i < n);
  if n = 1 then (*must be the case that i=0*) t
  else if i = 0 then in1 t (ntensor (n-1))
  else in2 Pauli (in_n_i (n-1) (i-1) t)

(*
  Given t : Pauli^n, return
  case t of {in_i q -> f i q}
*)
let rec match_in_i (n : int) (t : LambdaPC.Expr.t)
                   (f : int -> LambdaPC.Variable.t -> LambdaPC.Expr.t)
      : LambdaPC.Expr.t =
    assert (n > 0);
    let inc_f = fun j -> f (j + 1) in
    if n = 1
    then letin t (f 0)
    else caseof t (fun q0 -> f 0 q0) (fun q' -> match_in_i (n-1) (var q') inc_f)


let ptensor tp1 tp2 t1 t2 =
  (in1 t1 tp2) * (in2 tp1 t2)

let pauliNegX2Y3 = phase (const 1) (
    in_n_i 4 1 pauliX * in_n_i 4 2 pauliY
)
let pauliXY = in1 pauliX Pauli * in2 Pauli pauliY

let swap tp1 tp2 = lambda (PTensor (tp1, tp2)) (fun q ->
    caseof (var q)
      (fun q1 -> in2 tp1 (var q1))
      (fun q2 -> in1 (var q2) tp2)
  )

let cnot = lambda (PTensor (Pauli, Pauli)) (fun q ->
    caseof (var q)
      (fun q1 -> caseofP (var q1) 
                    (*Z->*) (in_n_i 2 0 pauliZ)
                    (*X->*) (in_n_i 2 0 pauliX * in_n_i 2 1 pauliX)
        )
      (fun q2 -> caseofP (var q2)
                    (*Z->*) (in1 pauliZ Pauli * in2 Pauli pauliZ)
                    (*X->*) (in2 Pauli pauliX)
        )
  )

(* Testing *)
(*Scalars (Z FIN2)*)
module S2 = Scalars.Scalars (Scalars.FIN2)
module Eval2 = LambdaPC.Eval(S2)
(* Check that hadamard Y = -Y *)

let printValuation e =
  print_endline (LambdaPC.Expr.pretty_string_of_t e ^ "\n->*\n");
  let result = Eval2.evalClosed e in
  print_endline (LambdaPC.Val.string_of_t result ^ "\n")

let evalTest () =

  print_endline ("-1 = " ^ S2.Zd.string_of_t (S2.Zd.t_of_int (-1)));
  let x = Eval2.cprod_phase (LambdaC.Val.Pair (LambdaC.Val.Const 1, LambdaC.Val.Const 0)) (LambdaC.Val.Pair (LambdaC.Val.Const 0, LambdaC.Val.Const 1)) in
  print_endline ("got " ^ S2.Zd.string_of_t x);

  print_endline "\n";
  printValuation (pauliY);
  printValuation pauliXY;
  printValuation (pauliNegX2Y3);
  printValuation (pow pauliX 1);
  printValuation (pow pauliZ 0);
  printValuation (hadamard @ pauliX);
  printValuation (hadamard @ (phase (const 1) pauliZ));
  printValuation (pauliZ * pauliX);
  printValuation (pauliX * pauliZ);
  printValuation (pauliX * pauliX);
  printValuation (hadamard @ pauliY);
  printValuation (qft @ pauliY);
  printValuation (swap Pauli Pauli @ pauliXY);
  printValuation (cnot @ pauliXY);
  (*error: printValuation (swap Pauli (ntensor 3) @ pauliNegX2Y3);*)
  print_endline "\n"