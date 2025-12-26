open Scalars

module SMT = struct
  open Smtml

  let pair e1 e2 = Expr.list [e1; e2]
  let fst tp e = Expr.unop tp Ty.Unop.Head e
  let snd tp e = fst tp (Expr.unop Ty.Ty_list Ty.Unop.Tail e)
  let lambda (x : Symbol.t) (e : Expr.t) = Smtml.Expr.list [Smtml.Expr.symbol x; e]

  let apply in_tp out_tp e1 e2 = 
    (* Assume e1 is a pair corresponding to an smt_lambda, e2 is a value *)
    let x = fst in_tp e1 in
    let e' = snd out_tp e1 in
    Expr.let_in [x; e2] e'

end

module SmtLambdaC (D : FIN) = struct
  open LambdaC

  let modd e = Smtml.Expr.binop Ty_int Rem e (Smtml.Expr.value (Int D.dim))
  let ( + ) e1 e2 = modd @@ Smtml.Expr.binop Ty_int Add e1 e2
  let ( * ) e1 e2 = modd @@ Smtml.Expr.binop Ty_int Mul e1 e2

  let smtml_of_type (tp : LambdaC.Type.t) : Smtml.Ty.t =
    match tp with
    | Unit -> Smtml.Ty.Ty_int
    | Sum (_, _) -> Smtml.Ty.Ty_list
    | Arrow (_, _) -> Smtml.Ty.Ty_list

  let var (ctx : Smtml.Expr.t VariableMap.t) (x : LambdaC.Variable.t) : Smtml.Expr.t =
    VariableMap.find x ctx
  let const (r : int) : Smtml.Expr.t = Smtml.Expr.value (Int (r mod D.dim))

  let lambda x e = SMT.lambda x e
  let apply tp1 tp2 e1 e2 = SMT.apply (smtml_of_type tp1) (smtml_of_type tp2) e1 e2

  let make_symbol tp = Smtml.Symbol.make (smtml_of_type tp) ("x")

  let rec zero (tp : Type.t) : Smtml.Expr.t =
    match tp with
    | Unit -> const 0
    | Sum (tp1, tp2) -> SMT.pair (zero tp1) (zero tp2)
    | Arrow(tp1, tp2) -> lambda (make_symbol tp1) (zero tp2)

  let fst tp1 e = SMT.fst (smtml_of_type tp1) e
  let snd tp2 e = SMT.snd (smtml_of_type tp2) e

  let rec plus (tp : Type.t) e1 e2 =
    match tp with
    | Unit -> e1 + e2
    | Sum(tp1, tp2) ->
        let e1' = plus tp1 (fst tp1 e1) (fst tp1 e2) in
        let e2' = plus tp2 (snd tp2 e1) (snd tp2 e2) in
        (SMT.pair e1' e2')
    | Arrow(tp1, tp2) ->
        let x = make_symbol tp1 in
        lambda x 
          (plus tp2 (apply tp1 tp2 e1 (Smtml.Expr.symbol x))
                    (apply tp1 tp2 e2 (Smtml.Expr.symbol x)))

  let rec scale (tp : Type.t) e e' =
    match tp with
    | Unit -> e * e'
    | Sum(tp1, tp2) ->
      let e1' = scale tp1 e (fst tp1 e') in
      let e2' = scale tp2 e (snd tp2 e') in
      SMT.pair e1' e2'
    | Arrow(tp1, tp2) ->
      let x = make_symbol tp1 in
      lambda x (scale tp2 e (apply tp1 tp2 e' (Smtml.Expr.symbol x)))

  let case tp1 tp2 tp' e x1 e1 x2 e2 =
    let v1 = fst tp1 e in
    let v2 = snd tp2 e in
    let e1' = apply tp1 tp' (lambda x1 e1) v1 in
    let e2' = apply tp2 tp' (lambda x2 e2) v2 in
    plus tp' e1' e2'
  
  exception TypeError
  let rec smtml_of_expr (ctx : Smtml.Expr.t VariableMap.t) (a : Expr.t) tp =
    match a with
    | Var x -> var ctx x
    | Zero tp -> zero tp
    | ZeroA (tp,_) -> zero tp
    | Annot (a, _) -> smtml_of_expr ctx a tp
    | Plus (e1, e2) -> plus tp (smtml_of_expr ctx e1 tp) (smtml_of_expr ctx e2 tp)
    | Const r -> const r
    | Scale (e1, e2) -> scale tp (smtml_of_expr ctx e1 Type.Unit) (smtml_of_expr ctx e2 tp)
    | Pair (e1, e2) ->
      ( match tp with
        | Sum (tp1, tp2) ->
          let e1' = smtml_of_expr ctx e1 tp1 in
          let e2' = smtml_of_expr ctx e2 tp2 in
          SMT.pair e1' e2'
        | _ -> raise TypeError
      )

    (* The annotations here are fairly restrictive, try to make this better *)
    | Case (Annot (e, Sum(tp1, tp2)), x1, e1, x2, e2) ->
      let x1' = make_symbol tp1 in
      let x2' = make_symbol tp2 in
      let ctx1 = VariableMap.add x1 (Smtml.Expr.symbol x1') ctx in
      let ctx2 = VariableMap.add x2 (Smtml.Expr.symbol x2') ctx in
      let e' = smtml_of_expr ctx e (Sum (tp1, tp2)) in
      let e1' = smtml_of_expr ctx1 e1 tp in
      let e2' = smtml_of_expr ctx2 e2 tp in
      case tp1 tp2 tp e' x1' e1' x2' e2'

    | Lambda (x,_,e') ->
      ( match tp with
        | Arrow (tp1, tp2) -> 
          let x' = make_symbol tp1 in
          let ctx' = VariableMap.add x (Smtml.Expr.symbol x') ctx in
          let e' = smtml_of_expr ctx' e' tp2 in
          lambda x' e'
        | _ -> raise TypeError
      )

    (* The annotations here are fairly restrictive, try to make this better *)
    | Apply (Annot (e1, Sum(tp1, tp2)), e2) ->
      apply tp1 tp2 (smtml_of_expr ctx e1 (Sum(tp1, tp2))) (smtml_of_expr ctx e2 tp1)


    | _ -> raise TypeError


end