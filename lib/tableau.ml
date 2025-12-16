open LambdaPC
open LambdaPC.HOAS

(* openining HOAS for access to constructors*)
(* FIN2 ( this is used in the test files) to evalulate over mod 2 scalers*)
module S2 = Scalars.Scalars (Scalars.FIN2)
module Eval2 = LambdaPC.Eval(S2)

(* a tableau is 2 lists of val.t ( which is a phase and the lambdac val )*)
(* the list has length n, stabilizers are for z outputs & destabilizers are for x outputs*)
type tab = 
{ stabilizers : LambdaPC.Val.t list
;destabilizers : LambdaPC.Val.t list}


(*  one quibit pauli as lambdaPC expr*)
let const = LambdaC.HOAS.const 
let pauli (r1 : int) (r2 : int) : LambdaPC.Expr.t = 
    vec ( LambdaC.Expr.Pair ( const r1, const r2))

    let pauliZ : LambdaPC.Expr.t = pauli 0 1 
    let pauliX : LambdaPC.Expr.t = pauli 1 0


(* nqubit pauli type *)
let rec ntensor ( n : int) : LambdaPC.Type.t = 
    if n =1 then Type.Pauli 
    else Type.PTensor ( Type.Pauli, ntensor ( n -1))


(* not super confident in this *)
(* in: int -> pauli -> pauli*)
(* placing a 1qubit into an nqubit*)
let rec pauli_at (n : int) ( i: int) ( t: LambdaPC.Expr.t): LambdaPC.Expr.t = 
    if n = 1 then t 
    else if i = 0 then in1 t (ntensor( n-1))
    else in2 Type.Pauli ( pauli_at( n-1) (i-1)t)
(* run and eval *)
    let apply_eval ( f: LambdaPC.Expr.pc) (arg : LambdaPC.Expr.t) : LambdaPC.Val.t = Eval2.evalClosed ( f @ arg)
(* for each i construct x, run f store the result repeat for z*)
let to_tablauu ( n : int) ( f : LambdaPC.Expr.pc) : tab = 
    let destabilizers = 
        List.init n ( fun i -> 
            apply_eval f ( pauli_at n i pauliX)
        )
    in 
    let stabilizers = 
        List.init n ( fun i -> 
                apply_eval f ( pauli_at n i pauliZ)
            )
    in 
    { stabilizers; destabilizers }





type pauli = LambdaPC.Expr.t
type tab = 
    { stabalizers : pauli list
    ; destabilizers : pauli list}
(* example of one qubit pauli tab for hadamard*)

let had : tab = 
      { stabilizers = [Interface.parse "X"]
      ; destabilizers = [Interface.parse "Z"]}
    


(* 
Parse x -> produces a 1 qubut pauli expression 
in i (parse "X") mebeds that into the full n-qubit system( so its a pauli on qubit i)
You can apply teh program foo ( a LambdaPC.Expr.pc) to that pauli expression
Evaluate the result with Interface.Eval2.evalClosed ( so it becomes a closed expression)
the resulting expression if again of type pauli 
then repeat process for z generators 
Package results into { stabilizers; destabilizers}
*)

