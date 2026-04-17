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

(*//////////////////
 /order of functions
///////////////////
val_to_list
 get_sqp_at
 reduce_strong
 reduce_sqp
 ss_to_ww
 sw_to_sn
 synthesize_row
 synthesis
*)


(* val to list*)
(* Maps a Val.t pair encoding to a list of (x,z) Pauli coordinates *)
  let rec val_to_list (v : LambdaC.Val.t) : (int * int) list =
  match v with
  | LambdaC.Val.Pair (LambdaC.Val.Const x, LambdaC.Val.Const z) -> [(x, z)]
  | LambdaC.Val.Pair (v1, v2) -> val_to_list v1 @ val_to_list v2
  | _ -> failwith "val_to_list: invalid format"

(* ///////////////////////////////////////////////////////////////////////// *)

(* Returns the (x,z) Pauli pair at qubit index l *)
(* get sqp at *) 
let get_sqp_at (l : int) (v : LambdaPC.Val.t) : (int * int) =
  let ls = val_to_list v.value in
  List.nth ls l

let reduce_weak (l : int) (pk : int * int) (qk : int * int) 
    (_gpk : int * int) (_gqk : int * int) : C.t =
  match (pk, qk) with
  (* add the goals within the match statements
  Look at ( pk, gpk) if those are not both the identy, then thast all you have to worry abt*)
  (* in that case that those are the identy, you  *)
  (* Case 1: (P, I) — bring to (X, I) *)
  | ((1,0),(0,0)) -> C.empty     (* X,I -> already there *)
  | ((1,1),(0,0)) -> [C.Sdg l]   (* Y,I -> X,I *)
  | ((0,1),(0,0)) -> [C.H l]     (* Z,I -> X,I *)
  (* Case 2: (I, Q) — bring to (I, X) *)
  | ((0,0),(1,0)) -> C.empty     (* I,X -> already there *)
  | ((0,0),(1,1)) -> [C.Sdg l]   (* I,Y -> I,X *)
  | ((0,0),(0,1)) -> [C.H l]     (* I,Z -> I,X *)
  (* Case 3: (P, P) — bring to (X, X) *)
  | ((1,0),(1,0)) -> C.empty     (* X,X -> already there *)
  | ((1,1),(1,1)) -> [C.Sdg l]   (* Y,Y -> X,X *)
  | ((0,1),(0,1)) -> [C.H l]     (* Z,Z -> X,X *)
  | _ -> failwith "reduce_weak: not a weak support"

(* ///////////////////////////////////////////////////////////////////////// *)

(*reduce_strong*)
(* Single-qubit Clifford gates to rotate strong support (pj,qj) -> (gpj,gqj) at qubit l *)
  let reduce_strong (l : int) (pj : int * int) (qj : int * int) (gpj : int * int) (gqj : int * int) : C.t =
    match (pj, qj) with
    (* i need to change the pattern matching *)
    (* I (0,0), Z (1,1), X ( 1,0) , Y ( 1,1) *)
  (* (X,Z) -> * *)
  | ((1,0),(0,1)) ->
    (match (gpj,gqj) with
    | ((1,0),(0,1)) -> C.empty                         (* X,Z -> X,Z *)
    | ((0,1),(1,0)) -> [C.H l]                         (* X,Z -> Z,X *)
    | ((1,1),(0,1)) -> [C.S l]                         (* X,Z -> Y,Z *)
    | ((0,1),(1,1)) -> [C.H l; C.S l]                  (* X,Z -> Z,Y *)
    | ((1,1),(1,0)) -> [C.Sdg l; C.H l]                (* X,Z -> Y,X *)
    | ((1,0),(1,1)) -> [C.H l; C.Sdg l; C.H l]         (* X,Z -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (Z,X) -> * *)
  | ((0,1),(1,0)) ->
    (match (gpj, gqj) with
    | ((1,0),(0,1)) -> [C.H l]                         (* Z,X -> X,Z *)
    | ((0,1),(1,0)) -> C.empty                         (* Z,X -> Z,X *)
    | ((1,1),(0,1)) -> [C.H l; C.S l]                  (* Z,X -> Y,Z *)
    | ((0,1),(1,1)) -> [C.H l; C.H l; C.S l]           (* Z,X -> Z,Y *)
    | ((1,1),(1,0)) -> [C.H l; C.Sdg l; C.H l]         (* Z,X -> Y,X *)
    | ((1,0),(1,1)) -> [C.H l; C.H l; C.Sdg l; C.H l]  (* Z,X -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (Y,Z) -> * *)
  | ((1,1),(0,1)) ->
    (match (gpj,gqj) with
    | ((1,0),(0,1)) -> [C.Sdg l]                             (* Y,Z -> X,Z *)
    | ((0,1),(1,0)) -> [C.Sdg l; C.H l]                     (* Y,Z -> Z,X *)
    | ((1,1),(0,1)) -> C.empty                               (* Y,Z -> Y,Z *)
    | ((0,1),(1,1)) -> [C.Sdg l; C.H l; C.S l]              (* Y,Z -> Z,Y *)
    | ((1,1),(1,0)) -> [C.Sdg l; C.Sdg l; C.H l]            (* Y,Z -> Y,X *)
    | ((1,0),(1,1)) -> [C.Sdg l; C.H l; C.Sdg l; C.H l]     (* Y,Z -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (Z,Y) -> * *)
  | ((0,1),(1,1)) ->
    (match (gpj,gqj) with
    | ((1,0),(0,1)) -> [C.Sdg l; C.H l]                          (* Z,Y -> X,Z *)
    | ((0,1),(1,0)) -> [C.Sdg l; C.H l; C.H l]                   (* Z,Y -> Z,X *)
    | ((1,1),(0,1)) -> [C.Sdg l; C.H l; C.S l]                   (* Z,Y -> Y,Z *)
    | ((0,1),(1,1)) -> C.empty                                    (* Z,Y -> Z,Y *)
    | ((1,1),(1,0)) -> [C.Sdg l; C.H l; C.Sdg l; C.H l]          (* Z,Y -> Y,X *)
    | ((1,0),(1,1)) -> [C.Sdg l; C.H l; C.H l; C.Sdg l; C.H l]   (* Z,Y -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (Y,X) -> * *)
  | ((1,1),(1,0)) ->
    (match (gpj,gqj) with
    | ((1,0),(0,1)) -> [C.H l; C.S l]                             (* Y,X -> X,Z *)
    | ((0,1),(1,0)) -> [C.H l; C.S l; C.H l]                     (* Y,X -> Z,X *)
    | ((1,1),(0,1)) -> [C.H l; C.S l; C.S l]                     (* Y,X -> Y,Z *)
    | ((0,1),(1,1)) -> [C.H l; C.S l; C.H l; C.S l]              (* Y,X -> Z,Y *)
    | ((1,1),(1,0)) -> C.empty                                    (* Y,X -> Y,X *)
    | ((1,0),(1,1)) -> [C.H l; C.S l; C.H l; C.Sdg l; C.H l]     (* Y,X -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  (* (X,Y) -> * *)
  | ((1,0),(1,1)) ->
    (match (gpj,gqj) with
    | ((1,0),(0,1)) -> [C.H l; C.S l; C.H l]                          (* X,Y -> X,Z *)
    | ((0,1),(1,0)) -> [C.H l; C.S l; C.H l; C.H l]                   (* X,Y -> Z,X *)
    | ((1,1),(0,1)) -> [C.H l; C.S l; C.H l; C.S l]                   (* X,Y -> Y,Z *)
    | ((0,1),(1,1)) -> [C.H l; C.S l; C.H l; C.H l; C.S l]            (* X,Y -> Z,Y *)
    | ((1,1),(1,0)) -> [C.H l; C.S l; C.H l; C.Sdg l; C.H l]          (* X,Y -> Y,X *)
    | ((1,0),(1,1)) -> C.empty                                         (* X,Y -> X,Y *)
    | _ -> failwith "reduce_strong: not a strong support")
  | _ -> failwith "reduce_strong: not a strong support"

  
(*/////////////////////*)
(*redce_weal *)
(* takes in k, pk, qk, gpk, gqk*)

(* output: a circuit c that takes in pk,qk, to gpk, gqk*)
(* if the soucr is x, and the gola is z then hadmamrd and so on and so forth *)
(* *)


(*
let reduce_sqp (l : int) (pj : int * int) (qj : int * int) : C.t =
  match (pj, qj) with
  (* Case 1: (P, I) — bring to (X, I) *)
  | ((1,0),(0,0)) -> C.empty          (* X,I -> already there *)
  | ((1,1),(0,0)) -> [C.Sdg l]        (* Y,I -> X,I *)
  | ((0,1),(0,0)) -> [C.H l]          (* Z,I -> X,I *)
  (* Case 2: (I, Q) — bring to (I, X) *)
  | ((0,0),(1,0)) -> C.empty          (* I,X -> already there *)
  | ((0,0),(1,1)) -> [C.Sdg l]        (* I,Y -> I,X *)
  | ((0,0),(0,1)) -> [C.H l]          (* I,Z -> I,X *)
  (* Case 3: (P, P) — bring to (X, X) *)
  | ((1,0),(1,0)) -> C.empty          (* X,X -> already there *)
  | ((1,1),(1,1)) -> [C.Sdg l]        (* Y,Y -> X,X *)
  | ((0,1),(0,1)) -> [C.H l]          (* Z,Z -> X,X *)
  | _ -> failwith "reduce_sqp: not a weak support"

  *)

(* detail in plain english sw_sn and it uses these helper functions*)
(*/////////////////////*)

(* 4 single qubits paulis *)
(* sw_sn_goal:  p2 , q2 : SQP): ( sqp sqp sqp sqp) *)
(* logic of sqp *)
(* if p2 (0,0) is the identy we are in case 2  ( z,i,x,x)*)

(* therefore the goal is z,i,x,x *)

(*if q2 (0,0) is the identy is we are in case 1 so the goal is ( x,x,z,i)
  ( p2 = p2, then case 3 x,x,y,x)
*) 
let sw_sn_goal (pk : int * int) (qk : int * int) 
    : (int * int) * (int * int) * (int * int) * (int * int) =
  match (pk, qk) with
  | ((0,0), _) -> ((0,1),(0,0),(1,0),(1,0))  (* case 2: pk=I → (Z,I,X,X) *)
  | (_, (0,0)) -> ((1,0),(1,0),(0,1),(0,0))  (* case 1: qk=I → (X,X,Z,I) *)
  | (p, q) when p = q -> ((1,0),(1,0),(1,1),(1,0))  (* case 3: pk=qk → (X,X,Y,X) *)
  | _ -> failwith "sw_sn_goal: not a weak support"


(*/////////////////////*)
(*ss_ww*)
(* Merges two strong supports at qubits j,k into two weak supports via CNOT(j,k) *)
let ss_to_ww (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t  =
  (* Find a circuit that takes (pj, qj) -> (X, Z) *)

  let pj = get_sqp_at j p in
  let qj = get_sqp_at j q in
  let pk = get_sqp_at k p in
  let qk = get_sqp_at k q in
  let gpj = (1,0) in
  let gqj = (1,1) in
  let gpk = (1,0) in  (* X *)
  let gqk = (0,1) in  (* Z *)
  let cj = reduce_strong j pj qj gpj gqj in
  let ck = reduce_strong k pk qk gpk gqk in

    C.concat (C.concat cj ck) [C.CNOT (j, k)]

(*/////////////////////*)

let sw_to_sn (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  let pj = get_sqp_at j p in
  let qj = get_sqp_at j q in
  let pk = get_sqp_at k p in
  let qk = get_sqp_at k q in
  let (gpj, gpk, gqj, gqk) = sw_sn_goal pk qk in
  let cj = reduce_strong j pj qj gpj gqj in
  let ck = reduce_weak k pk qk gpk gqk in
  C.concat (C.concat cj ck) [C.CNOT (j, k)]
(* sw_sn
(* Eliminates one weak support at qubit k, keeping strong support at j, via CNOT(j,k) *)
let sw_to_sn (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  (* Find a circuit that takes (p, q) -> (X, Z) — strong support on qubit j *)
  (* call sw_swn goals to get the goals *)
  (* we call get sqp at forst*)
  (* okay so the call of get sw_sn_ goal*)
  (* we have s on j, and weak)*)
  (*with strong support on.j and and no support on k. *)
  (* call reduce string *)
  let pj = get_sqp_at j p in
  let qj = get_sqp_at j q in
  let pk = get_sqp_at k p in
  let qk = get_sqp_at k q in
  let cj = reduce_strong j pj qj gpj gqj   in
(* needs update *)
  let ck = sqp_to_goal pk qk in
  (* CNOT*)
  C.concat (C.concat cj ck) [C.CNOT (j, k)]
  *)
(*/////////////////////////////////////////////////////////////////////////*)

(* synthesize_row *)
(*
(* Builds a circuit concentrating all support onto a single qubit, then swaps it to qubit i *)
let synthesize_row (i : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  let s = ref (count_strong p q) in (* amount of strong supports in (p,q) *)
  let w = ref (count_weak p q) in (* amount of weak supports in (p,q) *)
  let c = ref C.empty in (* empty, will be where gates are held*)
  let p = ref p in (* both p & q update as we index*)
  let q = ref q in
  while !s > 1 do
    let j = first_strong !p !q in (*first quibit that has ss *)
    let k = second_strong !p !q in (*2nd  quibit that has ss *)
    let (gates, p', q') = ss_to_ww j k !p !q in
    c := C.concat !c gates;
    p := p';
    (* := means what? *)
    (* allocation?*)
    q := q';

    s := !s - 2;
    w := !w + 2

  done;
  *)
let synthesize_row (i : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  let s = ref (count_strong p q) in
  let w = ref (count_weak p q) in
  let c = ref C.empty in
  let p = ref p in
  let q = ref q in

  (* Phase 1: more than one strong support *)
  while !s > 1 do
    let j = first_strong !p !q in
    let k = second_strong !p !q in
    let gates = ss_to_ww j k !p !q in
    c := C.concat !c gates;
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
(*/////////////////////////////////////////////////////////////////////////*)
(* Synthesis *)
(* Iterates synthesize_row over all stabilizer/destabilizer pairs, updating the tableau after each row *)
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




 









