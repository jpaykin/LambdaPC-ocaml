open Examples

open Tableau
module C = Circuit
module T = Tableau


(* stub/dummy functions*)
let count_strong _p _q = 0
let count_weak _p _q = 0
let first_strong _p _q = 0
let second_strong _p _q = 0
let first_weak _p _q = 0
let ss_to_ww _j _k _p _q = ([], _p, _q)
let sw_to_sn _j _k _p _q = (C.H 0, _p, _q)
(* Purpose: Produce a quantum circuit that maps the Pauli pair (P,Q) to the specific target operators (Zi​,Xi​) at qubit i.*)
(* reduce factor function*)

(*takes a target qubit i and two Pauli operators*)
(*
order of functions
val_to_list
get_sqp_at
reduce_strong
ss_to_ww
sw_to_sn
reduce_factor
synthesis
*)


(* val to list*)
(* val_to_list is being called on one lambdaC.val.t*)
  let rec val_to_list (v : LambdaC.Val.t) : (int * int) list =
  match v with
  | LambdaC.Val.Pair (LambdaC.Val.Const x, LambdaC.Val.Const z) -> [(x, z)]
  | LambdaC.Val.Pair (v1, v2) -> val_to_list v1 @ val_to_list v2
  | _ -> failwith "val_to_list: invalid format"

(*/////////////////////////////////////////////////////////////////////////*)
(* get sqp at *) 
let get_sqp_at (l : int) (v : LambdaPC.Val.t) : (int * int) =
  let ls = val_to_list v.value in
  List.nth ls l

(*/////////////////////////////////////////////////////////////////////////*)

(*reduce_strong*)
let reduce_strong (l : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  match (get_sqp_at l p, get_sqp_at l q) with
  | ((1,0), (1,1)) -> C.empty           (* X, Z *)
  | ((1,1), (1,0)) -> [C.H l]           (* Z, X *)
  | ((0,1), (1,1)) -> [C.Sdg l]         (* Y, Z *)
  | ((1,1), (0,1)) -> [C.Sdg l; C.H l]  (* Z, Y *)
  | ((0,1), (1,0)) -> [C.H l; C.S l]    (* Y, X *)
  | ((1,0), (0,1)) -> [C.H l; C.S l; C.H l] (* X, Y *)
   | _ -> failwith "reduce_strong: not a strong support"  (* add this *)
  

(*/////////////////////*)
(*ss_ww*)
let ss_to_ww (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t  =
  (* Find a circuit that takes (pj, qj) -> (X, Z) *)
(* don't need to add apply_eval*)  
  let cj = reduce_strong [j] p q in
  
(* Find a circuit that takes (pk, qk) -> (X, Z) *)
  let ck = reduce_strong [k] p q in 

  (* Apply CNOT *)
  let cnot_gate = C.CNOT (j, k) in
  (* Return cj gate applied to qubit j, ck gates applied to qubit k, and then a CNOT *)
  let circuit = C.concat (C.concat cj ck) [cnot_gate] in
  (circuit)

(*/////////////////////*)
(* sw_wn*)
let sw_to_wn (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  (* Find a circuit that takes (pj, qj) -> (X, Z) — strong support on qubit j *)
  let cj = reduce_strong [j] p q in
  (* Find a circuit that takes (pk, qk) -> (X, I) / (I, X) / (X, X) depending
     on which weak-support case applies (qk=I, pk=I, or pk=qk) — see Fig. 4 *)
  let ck = reduce_weak [k] p q in
  (* not just cnot *)
  (* Apply CNOT from j (control) to k (target) *)
  let cnot_gate = C.CNOT (j, k) in
  (* cj acts on qubit j, ck acts on qubit k, then CNOT eliminates k's support *)
  let circuit = C.concat (C.concat cj ck) [cnot_gate] in
  circuit


(*/////////////////////////////////////////////////////////////////////////*)
(*reduce factor*)
let reduce_factor (i : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  let s = ref (count_strong p q) in (* amount of strong supports in (p,q) *)
  let w = ref (count_weak p q) in (* amount of weak supports in (p,q) *)
  (* empty, will be where gates are held*)
  let c = ref C.empty in (* empty, will be where gates are held*)
  let p = ref p in (* both p & q update as we index*)
  let q = ref q in

  (* Phase 1: There is more than one strong support*)
  (*  This is SS_WW*)
  while !s > 1 do

    let j = first_strong !p !q in (*first quibit that has ss *)
    let k = second_strong !p !q in (*2nd  quibit that has ss *)

    (*RETURNS: gates, p` & q` *)
    (* This only needs to return the gates*)
    let (gates, p', q') = ss_to_ww j k !p !q in
    c := C.concat !c gates;
    p := p';
    (* := means what? *)
    (* allocation?*)
    q := q';

    (* updating counts*)
    s := !s - 2;
    w := !w + 2

  done;

(* Case 2: There is at least one Weak support *)
(* now handling weak support *)

  while !w > 0 do
(* this strong support doesnt change *)

    let j = first_strong !p !q in
    let k = first_weak !p !q in
  (* swn_sn will return one  2 qubit gate, but will need to return the circuit *)
    let (gate, p', q') = sw_to_sn j k !p !q in
    c := C.concat !c [gate];
    p := p';
    q := q';
    w := !w - 1
  done;

(* after both loops end we are left with one remaining qubit*)
  let j = first_strong !p !q in
(*check to see if this is the qubit we want, if not then we continue*)
    if j <> i then
(* apply a swap gate, to move the support from where it landed (J) *)
    c := C.concat !c [C.SWAP (i, j)];

!c
(* synthesis file *)
let synthesis (tab : T.tab) : C.t =
  let n = List.length tab.stabilizers in
  let tab = ref tab in
  let result = ref C.empty in

  for i = 0 to n - 1 do
    let (p, q) = get_row !tab i in
        let c = reduce_factor i p q in

    (* update the whole tableau by applying c to it *)
    tab := apply_circuit !tab c;

    (* accumulate: result = c1 ; c2 ; c3 ; ... *)
    result := C.concat !result c
  done;




  (* check every row is now (Zi, Xi)  *)
 

  !result
  (* ss-ww*)
  (* ss_to_ww j k p q — strong-strong to weak-weak
   Takes two qubits j and k that both have strong support in the factor p,q.
   Applies one of six specific TQE (two-qubit entangling) gates that turns both strong supports into weak supports.
   So after this, j and k both drop from strong to weak.
   Returns the circuit C such that (p,q) -> (p', q')
*)






