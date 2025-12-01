open LambdaPC


type pauli = LambdaPC.Expr.t
type tab = 
    { stabalizers : pauli list
    ; destabilizers : pauli list}
(* example of one qubit pauli tab for hadamard*)
let had : tab = 
      { stabalizers = [interface.parse "X"]
      ; destabilizers = [interface.parse "Z"]}
    
