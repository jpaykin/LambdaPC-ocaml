

val apply_circuit : Circuit.t -> Tableau.tab -> Tableau.tab
val apply_circuit_to_pauli : Circuit.t -> int -> LambdaPC.Val.t -> LambdaPC.Val.t
val synthesize_row : int -> LambdaPC.Val.t -> LambdaPC.Val.t -> Circuit.t
val synthesis : Tableau.tab -> Circuit.t