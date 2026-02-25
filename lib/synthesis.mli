(* produces a circuit C such that to_tableau(n, C) = to_tableau(n, f). *)
(*open LambdaPC*)
val synthesize :
(* n is the number of qubits *)
  n:int ->
  max_nodes:int ->
  max_depth:int ->
  (* TODO:  missing an open statement*)
  LambdaPC.Expr.pc -> Circuit.t option
