open Tableau
module C = Circuit
module T = Tableau

(* stub/dummy functions *)
let count_strong _p _q = 0
let count_weak _p _q = 0
let first_strong _p _q = 0
let second_strong _p _q = 0
let first_weak _p _q = 0

(* ///////////////////////////////////////////// 
   Goal: build a quantum circuit that maps a Pauli pair (P, Q)
   to the target operators (Zi, Xi) at qubit i.

   Functions order:
     val_to_list
     get_sqp_at
     reduce_weak
     reduce_strong
     sw_sn_goal
     ss_to_ww
     sw_to_sn
     apply_circuit
     apply_circuit_to_pauli
     synthesize_row
     synthesis
   ///////////////////////////////////////////// *)

(* val to list*)
(* Maps a nested pair Val.t encoding to a list of (x,z) Pauli coordinates *)
  let rec val_to_list (v : LambdaC.Val.t) : (int * int) list =
  match v with
  | LambdaC.Val.Pair (LambdaC.Val.Const x, LambdaC.Val.Const z) -> [(x, z)]
  | LambdaC.Val.Pair (v1, v2) -> val_to_list v1 @ val_to_list v2
  | _ -> failwith "val_to_list: invalid format"

(* get_sqp_at
 Returns the (x,z) Pauli coordinate pair at qubit index l, 
 from the list of pauli coordinates. *) 
let get_sqp_at (l : int) (v : LambdaPC.Val.t) : (int * int) =
  let ls = val_to_list v.value in
  List.nth ls l


(* reduce_weak
   Returns a single-qubit Clifford circuit that rotates a
   weak support qubit at index L from its current Pauli
   pair (pk, qk) toward the target form.

   Weak support cases:
     Case 1 — (P, I): rotates P -> X, leaves Q = I
     Case 2 — (I, Q): leaves P = I,  rotates Q -> X
     Case 3 — (P, P): rotates both  -> (X, X)

   Where: I=(0,0)  X=(1,0)  Y=(1,1)  Z=(0,1) *)
let reduce_weak (l : int) (pk : int * int) (qk : int * int) 
    (_gpk : int * int) (_gqk : int * int) : C.t =
  match (pk, qk) with
  (* Case 1: (P, I) — bring to (X, I) *)
  | ((1,0),(0,0)) -> C.empty     (* X,I -> X,I *)
  | ((1,1),(0,0)) -> [C.Sdg l]   (* Y,I -> X,I *)
  | ((0,1),(0,0)) -> [C.H l]     (* Z,I -> X,I *)
  (* Case 2: (I, Q) — bring to (I, X) *)
  | ((0,0),(1,0)) -> C.empty     (* I,X -> I,X *)
  | ((0,0),(1,1)) -> [C.Sdg l]   (* I,Y -> I,X *)
  | ((0,0),(0,1)) -> [C.H l]     (* I,Z -> I,X *)
  (* Case 3: (P, P) — bring to (X, X) *)
  | ((1,0),(1,0)) -> C.empty     (* X,X -> X,X *)
  | ((1,1),(1,1)) -> [C.Sdg l]   (* Y,Y -> X,X *)
  | ((0,1),(0,1)) -> [C.H l]     (* Z,Z -> X,X *)
  | _ -> failwith "reduce_weak: not a weak support"

(* reduce_strong
   Returns a single-qubit Clifford circuit that rotates a
   strong support qubit at index l from (pj, qj) to the 
   target (gpj, gqj).
   Strong support where pj and qj anticommute. 

   Where: I=(0,0)  X=(1,0)  Y=(1,1)  Z=(0,1) *)
  let reduce_strong (l : int) (pj : int * int) (qj : int * int) (gpj : int * int) (gqj : int * int) : C.t =
    match (pj, qj) with
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

(* sw_sn_goal
   Given a weak-support pair (pk, qk) at qubit k, returns the
   four target Paulis (gpj, gpk, gqj, gqk) for sw_to_sn. *)
let sw_sn_goal (pk : int * int) (qk : int * int) 
    : (int * int) * (int * int) * (int * int) * (int * int) =
  match (pk, qk) with
  | ((0,0), _) -> ((0,1),(0,0),(1,0),(1,0))  (* case 2: pk=I → (Z,I,X,X) *)
  | (_, (0,0)) -> ((1,0),(1,0),(0,1),(0,0))  (* case 1: qk=I → (X,X,Z,I) *)
  | (p, q) when p = q -> ((1,0),(1,0),(1,1),(1,0))  (* case 3: pk=qk → (X,X,Y,X) *)
  | _ -> failwith "sw_sn_goal: not a weak support"

(* ss_to_ww
   Reduces two strong support qubits j and k into two weak
   support qubits via CNOT(j, k).

   Steps:
     1. Rotate qubit j: (pj,qj) -> (X, Y)  using reduce_strong
     2. Rotate qubit k: (pk,qk) -> (X, Z)  using reduce_strong
     3. Apply CNOT(j, k)

   After CNOT, the strong support at j and k is reduced and
   replaced by two weak supports. *)
let ss_to_ww (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t  =
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

(* sw_to_sn
 takes a strong qubit j and a weak qubit k and eliminates
 the weak support at k via CNOT(j, k).
*)
let sw_to_sn (j : int) (k : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  let pj = get_sqp_at j p in
  let qj = get_sqp_at j q in
  let pk = get_sqp_at k p in
  let qk = get_sqp_at k q in
  let (gpj, gpk, gqj, gqk) = sw_sn_goal pk qk in
  let cj = reduce_strong j pj qj gpj gqj in
  let ck = reduce_weak k pk qk gpk gqk in
  C.concat (C.concat cj ck) [C.CNOT (j, k)]

(* apply_circuit
   Applies circuit c to every row of the tableau,
   returning an updated tableau. *)
let apply_circuit (c : C.t) (tab : T.tab) : T.tab =
  let n = List.length tab.stabilizers in
  let f = C.circuit_to_pc n c in
  {
    stabilizers   = List.map (fun p -> apply_eval f (LambdaPC.Val.expr_of_t p)) tab.stabilizers;
    destabilizers = List.map (fun p -> apply_eval f (LambdaPC.Val.expr_of_t p)) tab.destabilizers;
  }

(* apply_circuit_to_pauli
   Applies circuit c to a single Pauli p and returns the
   resulting Pauli value.
   Converts c to a λPC expression f (by circuit_to_pc),
   then evaluates f applied to p. *)
let apply_circuit_to_pauli (c : C.t) (n : int) (p : LambdaPC.Val.t) : LambdaPC.Val.t =
  let f = Circuit.circuit_to_pc n c in
  Interface.Eval2.evalClosed LambdaPC.HOAS.(f @ LambdaPC.Val.expr_of_t p)

(* synthesize_row
   Synthesizes the circuit that maps the i-th 
   Pauli pair (p, q) to (Zi, Xi) at qubit i.

   Method:
     Phase 1: Strong reduction:
       While there are ≥ 2 strong support qubits, pick the
       first two (j, k) and call ss_to_ww to merge them into
       two weak supports. Repeat until ≤ 1 strong support.

     Phase 2: Weak elimination:
       While there are weak support qubits, pick the strong
       qubit j and the first weak qubit k, call sw_to_sn to
       absorb k into j. Repeat until no weak supports remain.

     Phase 3:
       If the single remaining strong support is not at qubit
       i, apply SWAP(i, j) to move it into position. *)
let synthesize_row (i : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  
  let n = List.length (val_to_list p.value) in
  let s = ref (count_strong p q) in
  let w = ref (count_weak p q) in
  let c = ref C.empty in
  let p = ref p in
  let q = ref q in

  (* Phase 1: reduce strong support count to at most 1 *)
  while !s > 1 do
    let j = first_strong !p !q in
    let k = second_strong !p !q in
    let gates = ss_to_ww j k !p !q in
    c := C.concat !c gates;
    p := apply_circuit_to_pauli gates n !p;
    q := apply_circuit_to_pauli gates n !q;
    s := !s - 2;
    w := !w + 2
  done;

  (* Phase 2: eliminate all weak supports *)
  while !w > 0 do
    let j = first_strong !p !q in
    let k = first_weak !p !q in
    let gates = sw_to_sn j k !p !q in
    c := C.concat !c gates;
    p := apply_circuit_to_pauli gates n !p;
    q := apply_circuit_to_pauli gates n !q;
    w := !w - 1

  done;

(* Phase 3: move the remaining strong support to qubit i *)
  let j = first_strong !p !q in
    if j <> i then
    c := C.concat !c [C.SWAP (i, j)];
!c
(* synthesis
   Top-level method for synthesizing a circuit for an entire tableau.
   For each row i, calls synthesize_row to get the circuit that maps
   the i-th Pauli pair to (Zi, Xi) at qubit i, applies that circuit to 
   the tableau to update it, and concatenates the circuit to the result.
   Returns the concatenated circuit for all rows. *) 

let synthesis (tab : T.tab) : C.t =
  let n = List.length tab.stabilizers in
  let tab = ref tab in
  let result = ref C.empty in
  for i = 0 to n - 1 do
    let p = List.nth (!tab).stabilizers i in
    let q = List.nth (!tab).destabilizers i in
    let c = synthesize_row i p q in
    tab := apply_circuit c !tab ;
    result := C.concat !result c
  done;
  !result



