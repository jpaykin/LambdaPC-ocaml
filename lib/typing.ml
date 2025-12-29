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


  module Z3 = Smtml.Solver.Batch (Smtml.Z3_mappings)
  let solver = Z3.create ()
  type result =
      Equivalent
    | NotEquivalent of Model.t
    | Unknown

  let is_equiv tp e1 e2 : result =
    let cond = Expr.unop Ty_bool Not (Expr.relop tp Eq e1 e2) in
    Z3.add solver [ cond ];
    match Z3.check solver [] with
    | `Sat -> 
        let model = Z3.model solver |> Option.get in
        NotEquivalent model
    | `Unsat -> Equivalent
    | `Unknown -> Unknown

end

module SmtLambdaC (Zd : Z_SIG) = struct
  open LambdaC
  module EvalZd = Eval(Zd)

  let modd e = Smtml.Expr.binop Ty_int Rem e (Smtml.Expr.value (Int Zd.Dim.dim))
  let ( + ) e1 e2 = modd @@ Smtml.Expr.binop Ty_int Add e1 e2
  let ( * ) e1 e2 = modd @@ Smtml.Expr.binop Ty_int Mul e1 e2

  let smtml_of_type (tp : LambdaC.Type.t) : Smtml.Ty.t =
    match tp with
    | Unit -> Smtml.Ty.Ty_int
    | Sum (_, _) -> Smtml.Ty.Ty_list
    | Arrow (_, _) -> Smtml.Ty.Ty_list

  let var (ctx : Smtml.Symbol.t VariableMap.t) (x : LambdaC.Variable.t) : Smtml.Expr.t =
    Smtml.Expr.symbol @@ VariableMap.find x ctx
  let const (r : int) : Smtml.Expr.t = Smtml.Expr.value (Int (r mod Zd.Dim.dim))

  let lambda x e = SMT.lambda x e
  let apply tp1 tp2 e1 e2 = SMT.apply (smtml_of_type tp1) (smtml_of_type tp2) e1 e2

  let make_symbol tp x = Smtml.Symbol.make (smtml_of_type tp) ("x" ^ string_of_int x)
  let fresh_symbol tp = 
    let x = LambdaC.HOAS.fresh () in
    make_symbol tp x

  let rec zero (tp : Type.t) : Smtml.Expr.t =
    match tp with
    | Unit -> const 0
    | Sum (tp1, tp2) -> SMT.pair (zero tp1) (zero tp2)
    | Arrow(tp1, tp2) -> lambda (fresh_symbol tp1) (zero tp2)

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
        let x = fresh_symbol tp1 in
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
      let x = fresh_symbol tp1 in
      lambda x (scale tp2 e (apply tp1 tp2 e' (Smtml.Expr.symbol x)))

  let case tp1 tp2 tp' e x1 e1 x2 e2 =
    let v1 = fst tp1 e in
    let v2 = snd tp2 e in
    let e1' = apply tp1 tp' (lambda x1 e1) v1 in
    let e2' = apply tp2 tp' (lambda x2 e2) v2 in
    plus tp' e1' e2'
  
  exception TypeError of (string * Expr.t list * Type.t option)

  let rec smtml_of_expr (ctx : Smtml.Symbol.t VariableMap.t) (a : Expr.t) tp =
    match a with
    | Var x -> var ctx x
    | Let (Annot(a1, tp1),x,a2) ->

        let e1 = smtml_of_expr ctx a1 tp1 in
        let s = make_symbol tp1 x in
        let e2 = smtml_of_expr (VariableMap.add x s ctx) a2 tp in
        Smtml.Expr.let_in [Smtml.Expr.symbol s; e1] e2
    | Zero tp -> zero tp
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
        | _ -> raise @@ TypeError ("[smtml_of_expr] Expressions of the form (-,-) must have sum type", [a], Some tp)
      )

    (* The annotations here are fairly restrictive, try to make this better using the typechecker/type inference *)
    | Case (Annot (e, Sum(tp1, tp2)), x1, e1, x2, e2) ->
      let s1 = make_symbol tp1 x1 in
      let s2 = make_symbol tp2 x2 in
      let ctx1 = VariableMap.add x1 s1 ctx in
      let ctx2 = VariableMap.add x2 s2 ctx in
      let e' = smtml_of_expr ctx e (Sum (tp1, tp2)) in
      let e1' = smtml_of_expr ctx1 e1 tp in
      let e2' = smtml_of_expr ctx2 e2 tp in
      case tp1 tp2 tp e' s1 e1' s2 e2'

    | Lambda (x,_,e') ->
      ( match tp with
        | Arrow (tp1, tp2) -> 
          let s = make_symbol tp1 x in
          let ctx' = VariableMap.add x s ctx in
          let e' = smtml_of_expr ctx' e' tp2 in
          lambda s e'
        | _ -> raise @@ TypeError ("[smtml_of_expr] Expressions of the form Lambda(-,-,-) must have function type",
                                  [a],
                                  Some tp)
      )

    (* The annotations here are fairly restrictive, try to make this better *)
    | Apply (Annot (e1, Sum(tp1, tp2)), e2) ->
      apply tp1 tp2 (smtml_of_expr ctx e1 (Sum(tp1, tp2))) (smtml_of_expr ctx e2 tp1)

    | _ -> raise @@ TypeError ("[smtml_of_expr]", [a], Some tp)

  (*
  let rec free_variables (a : Expr.t) : LambdaC.UsageContext.t =
    match a with 
    | Var x -> UsageContext.singleton x
    | Zero _ -> UsageContext.empty (* ?? *)
    | ZeroA (_,ctx) -> ctx
    | Annot (a, _) -> free_variables a
    | Plus (a1,a2) -> free_variables a1 `union` free_variables a2
    | Const r -> UsageContext.empty
    | Scale (a1, a2) -> free_variables a1 `union` free_variables a2
    | Pair (a1, a2) -> free_variables a1 `union` free_variables a2
    | Case (a',x1,a1,x2,a2) ->
        let vars1 = remove x1 (free_variables a1) in
        let vars2 = remove x2 (free_variables a2) in
        free_variables a' `union` vars1 `union` vars2
    | Lambda (x,_,a') -> remove x (free_variables a')
    | Apply (a1,a2) -> free_variables a1 `union` free_variables a2
  *)

  let rec value_of_smtml (tp : Type.t) (v : Smtml.Value.t) : Val.t =
    match tp, v with
    | Unit, Int r -> Val.Const r
    | Sum (tp1,tp2), List [v1; v2] -> Val.Pair (value_of_smtml tp1 v1, value_of_smtml tp2 v2)
    | Arrow (_, _), List [_;_] -> 
      raise @@ TypeError ("[smtml] Cannot coerce smtml value to LambdaC value of function type", [], Some tp)
    | _, _ -> 
      raise @@ TypeError ("[smtml] Cannot find LambdaC value corresponding to the given smtml value", [], Some tp)

  module VariableMap = LambdaC.VariableMap
  (* When we encounter a free variable in a, add a binding to the corresponding symbol *)
  let make_symbol_map (ctx : LambdaC.Type.t VariableMap.t) : Smtml.Symbol.t VariableMap.t =
    let f x tp = make_symbol tp x in
    VariableMap.mapi f ctx


  let instantiate model (x : Variable.t) (tp : Type.t) : Val.t =
    let s = make_symbol tp x in
    let v0 = Smtml.Model.evaluate model s |> Option.get in
    value_of_smtml tp v0

  let counterexample_of_model (ctx : Type.t VariableMap.t) model : Val.t VariableMap.t =
    VariableMap.mapi (instantiate model) ctx
 
  
  type counterexample = {
    inputs : Val.t VariableMap.t;
    lhs : Val.t;
    rhs : Val.t
  }
  let equiv (tp : Type.t) (ctx : Type.t VariableMap.t) (a1 : Expr.t) (a2 : Expr.t) : (unit, counterexample) result =
    let ctx0 = make_symbol_map ctx in

    let e1 = smtml_of_expr ctx0 a1 tp in
    let e2 = smtml_of_expr ctx0 a2 tp in

    match SMT.is_equiv (smtml_of_type tp) e1 e2 with
    | Equivalent -> Ok ()
    | NotEquivalent model ->
        let counter = counterexample_of_model ctx model in
        let v1 = EvalZd.eval counter a1 in
        let v2 = EvalZd.eval counter a2 in
      Error 
        { inputs = counterexample_of_model ctx model;
        lhs = v1;
        rhs = v2
        }
    | Unknown -> raise @@ TypeError ("[SmtLambdaC.equiv] Solver timeout", [a1;a2], Some tp)
    
end



module SmtLambdaPC (S : SCALARS) = struct
  module SmtC = SmtLambdaC (S.Zd)

  let symplectic_check in_tp out_tp (f : LambdaPC.Expr.pc) : (unit, SmtC.counterexample) result =
    match f with
    | Lam(x,_,t) ->
        let in_tp' = LambdaPC.Type.ltype_of_t in_tp in

        (* create the expression lhs = omega(psiof(t)[x1/x],psiof(t)[x2/x])*)
        let a = LambdaPC.SymplecticForm.psi_of t in
        LambdaC.HOAS.update_env a;
        let x1 = LambdaC.HOAS.fresh () in
        let x2 = LambdaC.HOAS.fresh () in
        let a1 = LambdaC.Expr.rename_var x x1 a in
        let a2 = LambdaC.Expr.rename_var x x2 a in
        let lhs = LambdaPC.SymplecticForm.omega out_tp a1 a2 in

        (* create the expression rhs = omega(x1,x2) *)
        let rhs = LambdaPC.SymplecticForm.omega in_tp (LambdaC.HOAS.var x1) (LambdaC.HOAS.var x2) in

        (* check for equivalence *)
        let ctx = LambdaC.VariableMap.of_list [(x1,in_tp'); (x2,in_tp')] in
        
        SmtC.equiv LambdaC.Type.Unit ctx lhs rhs

      
end
