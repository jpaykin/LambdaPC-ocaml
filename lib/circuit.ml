type qubit = int

type gate =
  | H of qubit
  | S of qubit
  | Sdg of qubit
  | X of qubit
  | Z of qubit
  | CNOT of qubit * qubit  (* control, target *)

type t = gate list

let empty : t = []

let singleton (g : gate) : t = [g]

(* i think having a custom append function would be better for readability *)
let concat (c1 : t) (c2 : t) : t = c1 @ c2


let invert_gate = function
  | H q -> H q
  | S q -> Sdg q
  | Sdg q -> S q
  | X q -> X q
  | Z q -> Z q
  | CNOT (c, target) -> CNOT (c, target)

let dagger (c : t) : t =
  List.rev_map invert_gate c

let to_string_gate = function
  | H q -> Printf.sprintf "H %d" q
  | S q -> Printf.sprintf "S %d" q
  | Sdg q -> Printf.sprintf "Sdg %d" q
  | X q -> Printf.sprintf "X %d" q
  | Z q -> Printf.sprintf "Z %d" q
  | CNOT (c, target) -> Printf.sprintf "CNOT (%d,%d)" c target

let to_string (c : t) : string =
  match c with
  | [] -> "[]"
  | _ ->
      let body = String.concat "; " (List.map to_string_gate c) in
      "[ " ^ body ^ " ]"
