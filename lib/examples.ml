open LambdaPC.HOAS

let const = LambdaC.HOAS.const
let pauli r1 r2 = vec (LambdaC.Expr.Pair (const r1, const r2))

let pauliZ : LambdaPC.Expr.t = pauli 0 1
let pauliX : LambdaPC.Expr.t = pauli 1 0
let pauliY : LambdaPC.Expr.t = pauli 1 1
let pauliI : LambdaPC.Expr.t = pauli 0 0

let pauliI_ tp = vec (LambdaC.HOAS.zero (LambdaPC.Type.ltype_of_t tp))

let id tp = lambda tp (fun q -> q)
let hadamard = (*lambda Pauli (fun q -> caseofP q pauliZ pauliX)*)
  Interface.pc @@
    "lambda q : Pauli . case q of { X -> Z | Z -> X }"
let qft = (*lambda Pauli (fun q -> caseofP q pauliZ (pow pauliX (const (-1))))*)
  Interface.pc @@
    "lambda q : Pauli. case q of { Z -> X^{-1} | X -> Z }"
let phasegate = 
  Interface.pc @@
    "lambda q : Pauli. case q of { X -> Y | Z -> Z }"
let phasegate_dag =
  Interface.pc @@
    "lambda q : Pauli. case q of { X -> <1>Y | Z -> Z }"

exception IllFormedType

let rec ntensor (n : int) : LambdaPC.Type.t =
  assert (n >= 1);
  if n = 1 then Pauli
  else PTensor (Pauli, ntensor (n-1))

let rec in_ (n : int) (i : int) (t : LambdaPC.Expr.t) : LambdaPC.Expr.t =
  assert (n >= 1 && 0 <= i && i < n);
  if n = 1 then (*must be the case that i=0*) t
  else if i = 0 then in1 t (ntensor (n-1))
  else in2 Pauli (in_ (n-1) (i-1) t)
let in_i_j n i j : LambdaPC.Expr.pc =
  assert (0 <= i && i < n && 0 <= j && j < n && i <> j);
  lambda (PTensor (Pauli,Pauli)) (fun q ->
    caseof q
      (fun qi -> in_ n i qi)
      (fun qj -> in_ n j qj)
  )

(*
  Given t : Pauli^n, return
  case t of {in_i q -> f i q}
*)
let rec match_in_i (n : int) (t : LambdaPC.Expr.t)
                   (f : int -> LambdaPC.Expr.t -> LambdaPC.Expr.t)
      : LambdaPC.Expr.t =
    (*assert (n > 0);*)
    let inc_f = fun j -> f (j + 1) in
    if n = 1
    then letin t (f 0)
    else caseof t (fun q0 -> f 0 q0) (fun q' -> match_in_i (n-1) q' inc_f)


let in_pc n i (f : LambdaPC.Expr.pc) : LambdaPC.Expr.pc =
  assert (0 <= i && i < n);
  lambda (ntensor n) (fun q ->
    match_in_i n q (fun j q0 -> 
      if i=j then in_ n i (f @ q0)
      else pauliI_ (ntensor n)
    ))

let in_pc_i_j n i j (f : LambdaPC.Expr.pc) : LambdaPC.Expr.pc =
  assert (0 <= i && i < n && 0 <= j && j < n && i <> j);
  lambda (ntensor n) (fun q ->
    match_in_i n q (fun k q0 ->
      if      k = i then in_i_j n i j @ (f @ in1 q0 Pauli)
      else if k = j then in_i_j n i j @ (f @ in2 Pauli q0)
      else pauliI_ (ntensor n)
    ))

let ptensor tp1 tp2 t1 t2 =
  (in1 t1 tp2) * (in2 tp1 t2)

let pauliNegX2Y3 = phase (const 1) (
    in_ 4 1 pauliX * in_ 4 2 pauliY
)
let pauliXY = in1 pauliX Pauli * in2 Pauli pauliY

let swap tp1 tp2 = lambda (PTensor (tp1, tp2)) (fun q ->
    caseof q
      (fun q1 -> in2 tp2 q1)
      (fun q2 -> in1 q2 tp1)
  )

let input_type pc =
  match pc with
  | LambdaPC.Expr.Lam(_, tp, _) -> tp
let seq pc1 pc2 =
  lambda (input_type pc1) (fun q -> pc2 @ pc1 @ q)
let par pc1 pc2 =
  let tp1 = input_type pc1 in
  let tp2 = input_type pc2 in
  lambda (PTensor(tp1,tp2)) (fun q ->
    caseof q
      (fun q1 -> in1 (pc1 @ q1) tp2)
      (fun q2 -> in2 tp1 (pc2 @ q2))
    )

(* parser is not working right
let swap2 = Interface.pc @@ "lambda q : Pauli ** Pauli. case q of { in1 x -> (in2 x) | in2 y -> (in1 y) }"
*)

let cnot = lambda (PTensor (Pauli, Pauli)) (fun q ->
    caseof q
      (fun q1 -> caseofP q1
                    (*Z->*) (in_ 2 0 pauliZ)
                    (*X->*) (in_ 2 0 pauliX * in_ 2 1 pauliX)
        )
      (fun q2 -> caseofP q2
                    (*Z->*) (in1 pauliZ Pauli * in2 Pauli pauliZ)
                    (*X->*) (in2 Pauli pauliX)
        )
  )
  (*
let cnot = 
  Interface.pc @@ "lambda q : Pauli ** Pauli.
    case q of {
      in1 q1 -> case q1 of
                { X -> in1 X * in2 X
                | Z -> in1 Z
                }
    | in2 q2 -> case q2 of {
                  X -> in2 X
                | Z -> in1 Z * in2 Z
                }
    }
  "
  *)

let _bad_example = lambda Pauli (fun q ->
  caseofP q pauliZ pauliZ
  )

(* Testing *)
(*Scalars (Z FIN2)*)
module S2 = Scalars.Scalars (Scalars.FIN2)
module Eval2 = LambdaPC.Eval(S2)
open Interface


(* Testing type relations *)
module TypeChecker = Typing.SmtLambdaPC(S2)


let evalTest () =


  print_endline "\n";
  eval (pauliY);
  eval pauliXY;
  eval (pauliNegX2Y3);
  eval (pow pauliX (const 1));
  eval (pow pauliZ (const 0));
  eval (hadamard @ pauliX);
  eval (hadamard @ (phase (const 1) pauliZ));
  eval (pauliZ * pauliX);
  eval (pauliX * pauliZ);
  eval (pauliX * pauliX);
  eval (hadamard @ pauliY);
  eval (qft @ pauliY);
  eval (swap Pauli Pauli @ pauliXY);
  (*eval (cnot @ pauliXY);*)
  eval (in2 Pauli (in1 pauliX (PTensor (Pauli, Pauli))));
  eval (swap Pauli (ntensor 3) @ pauliNegX2Y3);

  eval (parseFromFile "lib/examples.pc");

  print_endline "\n"

  (* this is the pauli to clifford - takes an expressiion and makes it a lambdaPc *)
  (*val pauli_to_Clifford: LambdaPC.Type.t -> LambdaPC.Expr.t -> LambdaPC.Expr.pc *)
  let pauli_to_clifford tp p =
    lambda tp ( fun q -> 
      (* psi gets of rod of phase things and leaves u with just the vector *)
      (* omega acts on lambda c things which are ints. *)
      (* therefor pc needs tp be converted to c things *)
      phase (LambdaPC.SymplecticForm.omega tp (LambdaPC.SymplecticForm.psi_of p) (LambdaPC.SymplecticForm.psi_of q)) q )
        (*phase (LambdaPC.HOAS.omega tp p q) q )*)
        
      (* using the phase notation from the HOAS file, in lambda pc *)
      (* LambdaPC to cliffrord to a PC thing *)

  (* use this to implment to gate_pc in the sysnthesis file*)


  

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