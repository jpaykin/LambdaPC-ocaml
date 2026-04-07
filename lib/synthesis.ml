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
  - Unesting of pairs into a list 
get_sqp_at
- grabbing the index, and getting from the list 

reduce_strong
  - needs to remove the call to get_sqp at 
  - need to change the input 
  - chnage all 4 of the current input values from lpc to sqp ( int, int) 
reduce_sqp
  - needs to its header upddated 
  - and it its match statment changed
  - All based on figure 1 

ss_to_ww
- creates two smaller circuits adds a cnnot concats them 
sw_to_sn
- creates two smaller circuits adds a cnnot concats them
synthesize_row
synthesis
*)


(* val to list*)
(* val_to_list is being called on one lambdaC.val.t*)
  let rec val_to_list (v : LambdaC.Val.t) : (int * int) list =
  match v with
  | LambdaC.Val.Pair (LambdaC.Val.Const x, LambdaC.Val.Const z) -> [(x, z)]
  | LambdaC.Val.Pair (v1, v2) -> val_to_list v1 @ val_to_list v2
  | _ -> failwith "val_to_list: invalid format"

(* ///////////////////////////////////////////////////////////////////////// *)
(* get sqp at *) 
let get_sqp_at (l : int) (v : LambdaPC.Val.t) : (int * int) =
  let ls = val_to_list v.value in
  List.nth ls l

(* ///////////////////////////////////////////////////////////////////////// *)
(*
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
   *)
(* Question: *)
(*reduce_strong*)
(* commentig out old header
let reduce_strong (l : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) (gp : LambdaPC.Val.t) (gq : LambdaPC.Val.t) : C.t =
  match (get_sqp_at l p, get_sqp_at l q) with
  *)
  let reduce_strong (l : int) (pj : int * int) (qj : int * int) (gpj : int * int) (gqj : int * int) : C.t =
    match (pj, qj) with
  (* (X,Z) -> * *)
  | ((1,0),(1,1)) ->
    (match (get_sqp_at l gp, get_sqp_at l gq) with
    | ((1,0),(1,1)) -> C.empty                         (* X,Z -> X,Z *)
    | ((1,1),(1,0)) -> [C.H l]                         (* X,Z -> Z,X *)
    | ((0,1),(1,1)) -> [C.S l]                         (* X,Z -> Y,Z *)
    | ((1,1),(0,1)) -> [C.H l; C.S l]                  (* X,Z -> Z,Y *)
    | ((0,1),(1,0)) -> [C.Sdg l; C.H l]                (* X,Z -> Y,X *)
    | ((1,0),(0,1)) -> [C.H l; C.Sdg l; C.H l]         (* X,Z -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (Z,X) -> * *)
  | ((1,1),(1,0)) ->
    (match (get_sqp_at l gp, get_sqp_at l gq) with
    | ((1,0),(1,1)) -> [C.H l]                         (* Z,X -> X,Z *)
    | ((1,1),(1,0)) -> C.empty                         (* Z,X -> Z,X *)
    | ((0,1),(1,1)) -> [C.H l; C.S l]                  (* Z,X -> Y,Z *)
    | ((1,1),(0,1)) -> [C.H l; C.H l; C.S l]           (* Z,X -> Z,Y *)
    | ((0,1),(1,0)) -> [C.H l; C.Sdg l; C.H l]         (* Z,X -> Y,X *)
    | ((1,0),(0,1)) -> [C.H l; C.H l; C.Sdg l; C.H l]  (* Z,X -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (Y,Z) -> * *)
  | ((0,1),(1,1)) ->
    (match (get_sqp_at l gp, get_sqp_at l gq) with
    | ((1,0),(1,1)) -> [C.Sdg l]                             (* Y,Z -> X,Z *)
    | ((1,1),(1,0)) -> [C.Sdg l; C.H l]                     (* Y,Z -> Z,X *)
    | ((0,1),(1,1)) -> C.empty                               (* Y,Z -> Y,Z *)
    | ((1,1),(0,1)) -> [C.Sdg l; C.H l; C.S l]              (* Y,Z -> Z,Y *)
    | ((0,1),(1,0)) -> [C.Sdg l; C.Sdg l; C.H l]            (* Y,Z -> Y,X *)
    | ((1,0),(0,1)) -> [C.Sdg l; C.H l; C.Sdg l; C.H l]     (* Y,Z -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (Z,Y) -> * *)
  | ((1,1),(0,1)) ->
    (match (get_sqp_at l gp, get_sqp_at l gq) with
    | ((1,0),(1,1)) -> [C.Sdg l; C.H l]                          (* Z,Y -> X,Z *)
    | ((1,1),(1,0)) -> [C.Sdg l; C.H l; C.H l]                   (* Z,Y -> Z,X *)
    | ((0,1),(1,1)) -> [C.Sdg l; C.H l; C.S l]                   (* Z,Y -> Y,Z *)
    | ((1,1),(0,1)) -> C.empty                                    (* Z,Y -> Z,Y *)
    | ((0,1),(1,0)) -> [C.Sdg l; C.H l; C.Sdg l; C.H l]          (* Z,Y -> Y,X *)
    | ((1,0),(0,1)) -> [C.Sdg l; C.H l; C.H l; C.Sdg l; C.H l]   (* Z,Y -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (Y,X) -> * *)
  | ((0,1),(1,0)) ->
    (match (get_sqp_at l gp, get_sqp_at l gq) with
    | ((1,0),(1,1)) -> [C.H l; C.S l]                             (* Y,X -> X,Z *)
    | ((1,1),(1,0)) -> [C.H l; C.S l; C.H l]                     (* Y,X -> Z,X *)
    | ((0,1),(1,1)) -> [C.H l; C.S l; C.S l]                     (* Y,X -> Y,Z *)
    | ((1,1),(0,1)) -> [C.H l; C.S l; C.H l; C.S l]              (* Y,X -> Z,Y *)
    | ((0,1),(1,0)) -> C.empty                                    (* Y,X -> Y,X *)
    | ((1,0),(0,1)) -> [C.H l; C.S l; C.H l; C.Sdg l; C.H l]     (* Y,X -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (X,Y) -> * *)
  | ((1,0),(0,1)) ->
    (match (get_sqp_at l gp, get_sqp_at l gq) with
    | ((1,0),(1,1)) -> [C.H l; C.S l; C.H l]                          (* X,Y -> X,Z *)
    | ((1,1),(1,0)) -> [C.H l; C.S l; C.H l; C.H l]                   (* X,Y -> Z,X *)
    | ((0,1),(1,1)) -> [C.H l; C.S l; C.H l; C.S l]                   (* X,Y -> Y,Z *)
    | ((1,1),(0,1)) -> [C.H l; C.S l; C.H l; C.H l; C.S l]            (* X,Y -> Z,Y *)
    | ((0,1),(1,0)) -> [C.H l; C.S l; C.H l; C.Sdg l; C.H l]          (* X,Y -> Y,X *)
    | ((1,0),(0,1)) -> C.empty                                         (* X,Y -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  | _ -> failwith "reduce_strong: not a strong support"

  
(*/////////////////////*)
(* call sqp at before we run the match statement, include in the input statements not has a standlone *)
let reduce_sqp (l : int) (pj : int * int) (qj : int * int) : C.t =
  match (pj, qj) with
  (* Case 1: (P, I) — bring to (X, I) *)
  | ((1,0),(0,0)) -> C.empty          (* X,I -> already there *)
  | ((0,1),(0,0)) -> [C.Sdg l]        (* Y,I -> X,I *)
  | ((1,1),(0,0)) -> [C.H l]          (* Z,I -> X,I *)
  (* Case 2: (I, Q) — bring to (I, X) *)
  | ((0,0),(1,0)) -> C.empty          (* I,X -> already there *)
  | ((0,0),(0,1)) -> [C.Sdg l]        (* I,Y -> I,X *)
  | ((0,0),(1,1)) -> [C.H l]          (* I,Z -> I,X *)
  (* Case 3: (P, P) — bring to (X, X) *)
  | ((1,0),(1,0)) -> C.empty          (* X,X -> already there *)
  | ((0,1),(0,1)) -> [C.Sdg l]        (* Y,Y -> X,X *)
  | ((1,1),(1,1)) -> [C.H l]          (* Z,Z -> X,X *)
  | _ -> failwith "reduce_sqp: not a weak support"
(*/////////////////////*)
(*ss_ww*)
let ss_to_ww (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t  =
  (* Find a circuit that takes (pj, qj) -> (X, Z) *)
  (* don't need to add apply_eval*)

  (* copying the use of get_sqp at from sw_sn*)
  let pj = get_sqp_at j p in
  let qj = get_sqp_at j q in
  let pk = get_sqp_at k p in
  let qk = get_sqp_at k q in

  let cj = reduce_strong j pj qj gpj gqj in
  let ck = reduce_strong k pk qk gpk gqk in
  (* Apply CNOT *)
  (* need to correctly refernece CNOT*)
  let cnot_gate = C.CNOT (j, k) in
  (* Return cj gate applied to qubit j, ck gates applied to qubit k, and then a CNOT *)
  let circuit = C.concat (C.concat cj ck) [cnot_gate] in
  (circuit)

(*/////////////////////*)
(* sw_sn*)
let sw_to_sn (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  (* extraction happens here *)

  (* Find a circuit that takes (p, q) -> (X, Z) — strong support on qubit j *)
  (* and weak support on k*)
  let pj = get_sqp_at j p in
  let qj = get_sqp_at j q in
  let pk = get_sqp_at k p in
  let qk = get_sqp_at k q in

  let cj = reduce_strong j pj qj  in
  let ck = reduce_sqp k pk qk in


  (* Find a circuit that takes (pk, qk) -> (X, I) / (I, X) / (X, X) depending
     on which weak-support case applies (qk=I, pk=I, or pk=qk) *)
     (* with strong*)
  
  (* CNOT*)
  C.concat cj ck
  (* needs to add CNOT at the end of the circuit*)


(*/////////////////////////////////////////////////////////////////////////*)
(*reduce factor*)
let synthesize_row (i : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
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
    let p = List.nth !tab.stabilizers i in
    let q = List.nth !tab.destabilizers i in
    let c = synthesize_row i p q in
    tab := apply_circuit !tab c;
    result := C.concat !result c
  done;
  !result



  (* check every row is now (Zi, Xi)  *)
 

  !result







