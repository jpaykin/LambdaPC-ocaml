open LambdaPC.HOAS

let const = LambdaC.HOAS.const
let pauli r1 r2 = vec (LambdaC.Expr.Pair (const r1, const r2))

let pauliZ : LambdaPC.Expr.t = pauli 0 1
let pauliX : LambdaPC.Expr.t = pauli 1 0
let pauliY : LambdaPC.Expr.t = pauli 1 1
let pauliI : LambdaPC.Expr.t = pauli 0 0

let pauliI_ tp = vec (LambdaC.HOAS.zero tp)

let id tp = lambda tp (fun q -> q)
let hadamard = lambda Pauli (fun q -> caseofP q pauliZ pauliX)
let qft = lambda Pauli (fun q -> caseofP q pauliZ (pow pauliX (-1)))
let phasegate = lambda Pauli (fun q ->
    caseofP q
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
                   (f : int -> LambdaPC.Expr.t -> LambdaPC.Expr.t)
      : LambdaPC.Expr.t =
    assert (n > 0);
    let inc_f = fun j -> f (j + 1) in
    if n = 1
    then letin t (f 0)
    else caseof t (fun q0 -> f 0 q0) (fun q' -> match_in_i (n-1) q' inc_f)


let ptensor tp1 tp2 t1 t2 =
  (in1 t1 tp2) * (in2 tp1 t2)

let pauliNegX2Y3 = phase (const 1) (
    in_n_i 4 1 pauliX * in_n_i 4 2 pauliY
)
let pauliXY = in1 pauliX Pauli * in2 Pauli pauliY

let swap tp1 tp2 = lambda (PTensor (tp1, tp2)) (fun q ->
    caseof q
      (fun q1 -> in2 tp2 q1)
      (fun q2 -> in1 q2 tp1)
  )

let cnot = lambda (PTensor (Pauli, Pauli)) (fun q ->
    caseof q
      (fun q1 -> caseofP q1
                    (*Z->*) (in_n_i 2 0 pauliZ)
                    (*X->*) (in_n_i 2 0 pauliX * in_n_i 2 1 pauliX)
        )
      (fun q2 -> caseofP q2
                    (*Z->*) (in1 pauliZ Pauli * in2 Pauli pauliZ)
                    (*X->*) (in2 Pauli pauliX)
        )
  )

(* Testing *)
(*Scalars (Z FIN2)*)
module S2 = Scalars.Scalars (Scalars.FIN2)
module Eval2 = LambdaPC.Eval(S2)
open Interface


let evalTest () =


  print_endline "\n";
  eval (pauliY);
  eval pauliXY;
  eval (pauliNegX2Y3);
  eval (pow pauliX 1);
  eval (pow pauliZ 0);
  eval (hadamard @ pauliX);
  eval (hadamard @ (phase (const 1) pauliZ));
  eval (pauliZ * pauliX);
  eval (pauliX * pauliZ);
  eval (pauliX * pauliX);
  eval (hadamard @ pauliY);
  eval (qft @ pauliY);
  eval (swap Pauli Pauli @ pauliXY);
  eval (cnot @ pauliXY);
  eval (in2 Pauli (in1 pauliX (PTensor (Pauli, Pauli))));
  eval (swap Pauli (ntensor 3) @ pauliNegX2Y3);

  eval (parseFromFile "lib/examples.pc");

  print_endline "\n"

  (*
  
let phasegate = lambda q : Pauli.
    case q of
    { X -> Y
    | Z -> Z
    } in

let pauliNegXY = <1> [I, [X, [Y, I]]] in
let pauliXY = in1{Pauli} X * in2{Pauli} Y in

let swap11 = lambda q : Pauli ** Pauli.
    case q of
    { in1 q1 -> in2{Pauli} q1
    | in2 q2 -> in1{Pauli} q2
    }
    in

let cnot = lambda q : Pauli ** Pauli .
    case q of
    { in1 q1 -> case q1 of 
                { X -> [X, X]
                | Z -> [Z, I]
                }
    | in2 q2 -> case q2 of 
                { X -> [I, X]
                | Z -> [Z, Z]
                }
    }
    in

cnot @ pauliXY
*)