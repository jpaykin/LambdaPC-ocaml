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

let reduce_factor (i : int) (p : LambdaPC.Val.t) (q : LambdaPC.Val.t) : C.t =
  let s = ref (count_strong p q) in (* amount of strong supports in (p,q) *)
  let w = ref (count_weak p q) in (* amount of weak supports in (p,q) *)
  (* empty, will be where gates are held*)
  let c = ref C.empty in (* empty, will be where gates are held*)
  let p = ref p in (* both p & q update as we index*)
  let q = ref q in

  (* Phase 1: There is more than one strong support*)
  while !s > 1 do

    let j = first_strong !p !q in (*first quibit that has ss *)
    let k = second_strong !p !q in (*2nd  quibit that has ss *)

    (*RETURNS: gates, p` & q` *)
    let (gates, p', q') = ss_to_ww j k !p !q in
    c := C.concat !c gates;
    p := p';
    q := q';

    (* updating counts*)
    s := !s - 2;
    w := !w + 2

  done;

(* Case 2: There is at least one Weak support *)
(* now handling weak support *)

  while !w > 0 do
(* this strong support doesnt change*)

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

