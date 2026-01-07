
open Examples

type tab =
  { stabilizers   : LambdaPC.Val.t list
  ; destabilizers : LambdaPC.Val.t list
  }

let apply_eval (f : LambdaPC.Expr.pc) (arg : LambdaPC.Expr.t) : LambdaPC.Val.t =
  Eval2.evalClosed (LambdaPC.HOAS.(f @ arg))

let to_tableau (n : int) (f : LambdaPC.Expr.pc) : tab =
  let destabilizers =
    List.init n (fun i -> apply_eval f (in_n_i n i pauliX))
  in
  let stabilizers =
    List.init n (fun i -> apply_eval f (in_n_i n i pauliZ))
  in
  { stabilizers; destabilizers }

let had : tab =
  { stabilizers   = [Eval2.evalClosed (Interface.parse "X")]
  ; destabilizers = [Eval2.evalClosed (Interface.parse "Z")] }
