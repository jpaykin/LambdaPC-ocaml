
type pauli = LambdaPC.Expr.t
type tab = 
    { stabalizers : pauli list
    ; destabilizers : pauli list}
(* example of one qubit pauli tab for hadamard*)
let had : tab = 
      { stabalizers = [Interface.parse "X"]
      ; destabilizers = [Interface.parse "Z"]}
    
