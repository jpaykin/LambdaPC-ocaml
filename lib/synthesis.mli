(* produces a circuit C such that to_tableau(n, C) = to_tableau(n, f). *)
(*open LambdaPC
val synthesize :
(* n is the number of qubits *)
  n:int ->
  max_nodes:int ->
  max_depth:int ->
  (* TODO:  missing an open statement*)
  LambdaPC.Expr.pc -> Circuit.t option
  *)

val apply_circuit : Circuit.t -> Tableau.tab -> Tableau.tab
val apply_circuit_to_pauli : Circuit.t -> int -> LambdaPC.Val.t -> LambdaPC.Val.t
val synthesize_row : int -> LambdaPC.Val.t -> LambdaPC.Val.t -> Circuit.t
val synthesis : Tableau.tab -> Circuit.t