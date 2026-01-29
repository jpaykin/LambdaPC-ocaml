open Examples
open Tableau
(* Using predefined gates, renames for readability*)

module T = Tableau
module C = Circuit

(*Composition of projective cliffords*)
(*Projective cliffords are functions on pauli operators*)
(* Creates 2 PC functions*)
let compose_pc (tp : LambdaPC.Type.t)
    (g : LambdaPC.Expr.pc)
    (f : LambdaPC.Expr.pc)
  : LambdaPC.Expr.pc =
  LambdaPC.HOAS.(lambda tp (fun x -> g @ (f @ (var x))))

(* Identity projective Clifford on n qubits *)
(* Maps pauli operators to themselves 
 this is the goal state  as
 we stop searching when the transformation equals the identy*)
let id_pc (n : int) : LambdaPC.Expr.pc =
  Examples.id (Examples.ntensor n)

(* Converts a single gate into a PC function*)
(* search works on transformations of pauli operators NOT Circuits*)
(* we get to a gate, then looks to determine how does this gate transform pauli ops*)
(* This is the functionality of how circuit gates act in the tableau space*)

(* updated this - to be for all gates*)
let gate_to_pc ~(n:int) (g : C.gate) : LambdaPC.Expr.pc =
  (* injector function *)
  match (n, g) with
  | (1, C.H _)   -> Examples.hadamard
  | (1, C.S _)   -> Examples.phasegate
  | (1, C.Sdg _) -> Exammples.sdg (* check that u dont need to call dagger on this*)
  | (1, C.X _) ->  Examples.pauli_x
  | (1, C.Z _) -> Examples.pauli_z
  | (_, C.CNOT (_, _)) -> Examples.cnot
  | _ -> 
      failwith "gate_to_pc: unsupported gate or qubit count"
(* remembers visited states*)
(* creates a unique string key for each tableau*)
(* prevents infinite looping, will skip over previously visited states*)

let key_of_tab (tab : T.tab) : string =
  let open LambdaPC.Val in
  let s =
    List.map string_of_t tab.stabilizers
    |> String.concat "|"
  in
  let d =
    List.map string_of_t tab.destabilizers
    |> String.concat "|"
  in
  s ^ "||" ^ d

(* one search state - represents one point in search graph*)
(* f : the current projective Clifford function
   tab :  tableau representation of f 
   circ : gates applied to reach this state
   depth : current search depth
 *)

type node =
  { f    : LambdaPC.Expr.pc
  ; tab  : T.tab
  ; circ : C.t
  ; depth : int
  }
  (* synthesis algorithm *)
(*
let synthesize
    ~(n:int)
    ~(max_nodes:int)
    ~(max_depth:int)
    (target : LambdaPC.Expr.pc)
  : C.t option =

  let tp = Examples.ntensor n in
  (* this is the identity tableau  which is our goal*)
  let goal_tab = T.to_tableau n (id_pc n) in
  (*  this is the starting tableau *)
  let start_tab = T.to_tableau n target in

  if key_of_tab start_tab = key_of_tab goal_tab then
    Some C.empty
  else
    let gates =
      let oneq =
        List.concat (List.init n (fun q -> [C.H q; C.S q]))
      in
      let twoq =
        List.concat (List.init n (fun i ->
          List.init n (fun j ->
            if i = j then None else Some (C.CNOT (i,j)))
          |> List.filter_map Fun.id))
      in
      oneq @ twoq
    

    let queue = Queue.create () in
    let visited = Hashtbl.create 100003 in

    let push nd =
      if nd.depth <= max_depth then
        let k = key_of_tab nd.tab in
        if not (Hashtbl.mem visited k) then begin
          Hashtbl.add visited k ();
          Queue.add nd queue
        end
    in

    push { f = target; tab = start_tab; circ = C.empty; depth = 0 };

    let rec loop expanded =
      if expanded >= max_nodes || Queue.is_empty queue then
        None
      else
        let cur = Queue.take queue in
        if key_of_tab cur.tab = key_of_tab goal_tab then
          (* would i want the foward gates in reverse order? *)
          (* can i just reverse the circuit? *)
          (* found a solution ( reverse order and invert each gate ) *)
          Some (C.dagger cur.circ)
        else begin
        List.iter (fun g ->
          let g_pc = gate_to_pc ~n g in
          let new_f = compose_pc tp g_pc cur.f in
          let new_tab = T.to_tableau n new_f in
          let new_circ = C.concat cur.circ [g] in
          push { f = new_f; tab = new_tab; circ = new_circ; depth = cur.depth + 1 }
          ) gates;
          loop (expanded + 1)
        end
    
    in

    loop 0
    *)
let synthesize
    ~(n:int)
    ~(max_nodes:int)
    ~(max_depth:int)
    (start_tab : T.tab)
  : C.t option =

  let goal_tab = T.to_tableau n (id_pc n) in

  if key_of_tab start_tab = key_of_tab goal_tab then
    Some C.empty
  else begin
    let gates =
      let oneq =
        List.concat (List.init n (fun q -> [C.H q; C.S q]))
      in
      let twoq =
        List.concat (List.init n (fun i ->
          List.init n (fun j ->
            if i = j then None else Some (C.CNOT (i,j)))
          |> List.filter_map Fun.id))
      in
      oneq @ twoq
    in

    let queue = Queue.create () in
    let visited = Hashtbl.create 100003 in

    let push (tab, circ, depth) =
      if depth <= max_depth then
        let k = key_of_tab tab in
        if not (Hashtbl.mem visited k) then begin
          Hashtbl.add visited k ();
          Queue.add (tab, circ, depth) queue
        end
    in

    push (start_tab, C.empty, 0);

    let rec loop expanded =
      if expanded >= max_nodes || Queue.is_empty queue then
        None
      else
        let (tab, circ, depth) = Queue.take queue in
        if key_of_tab tab = key_of_tab goal_tab then
          Some (C.dagger circ)
        else begin
          List.iter (fun g ->
            (* okay so this i think is where apply_gate would go *)
(*            let tab_prime  = T.apply_gate tab g in 
            let circ_prime = C.concat circ [g] in
            push (tab_prime, circ_prime, depth + 1)
          ) gates;
          loop (expanded + 1)
          *)
        end
    in
    loop 0
  end
