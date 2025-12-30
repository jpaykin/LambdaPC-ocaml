open Scalars
open LambdaPC

module VariableSet = LambdaC.VariableSet
module TypeInformation = LambdaC.TypeInformation
open TypeInformation

module LinearityTyping = struct
  type type_information = (LambdaPC.Type.t, LambdaPC.Expr.t) LambdaC.TypeInformation.t
  let string_of_info = TypeInformation.string_of_info Type.string_of_t Expr.pretty_string_of_t
  let pp_info info = print_string @@ string_of_info info
  let assert_type = TypeInformation.assert_type Type.string_of_t

  let assert_pc_type (tp : LambdaC.Type.t) : Type.t =
    match Type.t_of_ltype tp with
    | Some tp' -> tp'
    | None ->
      terr @@ "Expected a LambdaPC-compatible type, received: " ^ LambdaC.Type.string_of_t tp

  let assert_unit_type (tp : LambdaC.Type.t) : unit =
    match tp with
    | Unit -> ()
    | _ -> terr @@ "Expected a unit type, received: " ^ LambdaC.Type.string_of_t tp

  let assert_ptensor_type (tp : Type.t) : Type.t * Type.t =
    match tp with
    | PTensor(tp1,tp2) -> (tp1,tp2)
    | _ -> terr @@ "Expected a PTensor type, received: " ^ Type.string_of_t tp

  let rec typecheck' (ctx : Type.t VariableMap.t) (e : Expr.t) : type_information =
    match e with
    | Var x ->
      {
        expr = Var x;
        tp = type_of_var ctx x;
        usage = var_usage x
      }
    | Annot(e',tp) ->
      let info' = typecheck' ctx e' in
      assert_type tp info'.tp;
      {
        expr = Annot(info'.expr, tp);
        tp = tp;
        usage = info'.usage
      }
    | Let (e1,x,e2) ->
      let info1 = typecheck' ctx e1 in
      let ctx' = VariableMap.add x info1.tp ctx in
      let info2 = typecheck' ctx' e2 in
      {
        expr = Let(Annot(info1.expr, info1.tp), x, info2.expr);
        tp = info2.tp;
        usage = disjoint_usage_with info1 x info2
      }
    | LExpr a ->
      let info' = typecheckC ctx a in
      let tp' = assert_pc_type info'.tp in
      {
        expr = LExpr info'.expr;
        tp = tp';
        usage = info'.usage
      }
    | Phase (a1,e2) ->
      let info1 = typecheckC ctx a1 in
      let info2 = typecheck' ctx e2 in
      assert_unit_type info1.tp;
      {
        expr = Phase(info1.expr, info2.expr);
        tp = info2.tp;
        usage = same_usage info1 info2
      }

    | Prod (e1,e2) ->
      let info1 = typecheck' ctx e1 in
      let info2 = typecheck' ctx e2 in
      assert_type info1.tp info2.tp;
      {
        expr = Prod(info1.expr, info2.expr);
        tp = info1.tp;
        usage = same_usage info1 info2
      }
    | Pow (e1, a2) ->
      let info1 = typecheck' ctx e1 in
      let info2 = typecheckC ctx a2 in
      assert_unit_type info2.tp;
      {
        expr = Pow(info1.expr, info2.expr);
        tp = info1.tp;
        usage = disjoint_usage info1 info2
      }
    | CasePauli(e0,ex,ez) ->
      let info0 = typecheck' ctx e0 in
      let infox = typecheck' ctx ex in
      let infoz = typecheck' ctx ez in
      assert_type Pauli info0.tp;
      assert_type infox.tp infoz.tp;
      {
        expr = CasePauli (Annot(info0.expr, Pauli), infox.expr, infoz.expr);
        tp = infox.tp;
        usage = fun u_in u_out ->
          VariableSet.exists_usage_subset u_in (fun u_mid ->
            info0.usage u_in u_mid
            && infox.usage u_mid u_out
            && infoz.usage u_mid u_out
            )
      }
    | In1 (e1,tp2) ->
      let info1 = typecheck' ctx e1 in
      {
        expr = In1(info1.expr,tp2);
        tp = PTensor(info1.tp,tp2);
        usage = info1.usage
      }
    | In2 (tp1,e2) ->
      let info2 = typecheck' ctx e2 in
      {
        expr = In2(tp1,info2.expr);
        tp = PTensor(tp1,info2.tp);
        usage = info2.usage
      }
    | CasePTensor (e0,x1,e1,x2,e2) ->
      let info0 = typecheck' ctx e0 in
      let (tp1,tp2) = assert_ptensor_type info0.tp in
      let info1 = typecheck' (VariableMap.add x1 tp1 ctx) e1 in
      let info2 = typecheck' (VariableMap.add x2 tp2 ctx) e2 in
      assert_type info1.tp info2.tp;
      {
        expr = CasePTensor(Annot(info0.expr,info0.tp), x1, info1.expr, x2, info2.expr);
        tp = info1.tp;
        usage = disjoint_usage_branch info0 x1 info1 x2 info2
      }
    | Apply(pc1,e2) ->
      let info1 = typecheck_pc ctx pc1 in
      let info2 = typecheck' ctx e2 in
      let (tp1,tp2) = info1.tp in
      assert_type tp1 info2.tp;
      {
        expr = Apply(info1.expr, info2.expr);
        tp = tp2;
        usage = disjoint_usage info1 info2 (* is this right? *)
      }
    | Force (Suspend e') -> typecheck' ctx e'
  and typecheckC (ctx : Type.t VariableMap.t) (a : LambdaC.Expr.t) =
    LambdaC.Typing.typecheck' (VariableMap.map Type.ltype_of_t ctx) a
  and typecheck_pc ctx pc =
    let (Lam(x,tp,t)) = pc in
    let info = typecheck' (VariableMap.add x tp ctx) t in
    {
      expr = Lam(x,tp,info.expr);
      tp = (tp,info.tp);
      usage = fun u1 u2 ->
        not (VariableSet.mem x (VariableSet.union u1 u2))
        && info.usage (VariableSet.add x u1) u2
    }

  let linearity_check (e : Expr.t) : type_information =
    let info = typecheck' VariableMap.empty e in
    (* linearity check implies info.usage(0,0) *)
    match info.usage VariableSet.empty VariableSet.empty with
    | true -> info
    | false ->
      terr @@ "Linearity check failed in the usage relation.\n" ^ string_of_info info

  let linearity_check_pc (pc : Expr.pc) =
    let info = typecheck_pc VariableMap.empty pc in
    match info.usage VariableSet.empty VariableSet.empty with
    | true -> info
    | false ->
      terr @@ "Linearity check failed in the usage relation.\n" 
      ^ TypeInformation.string_of_info
        (fun (tp1,tp2) -> "|" ^ Type.string_of_t tp1 ^ " -o " ^ Type.string_of_t tp2 ^ "|")
        Expr.pretty_string_of_pc
        info
end

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
  
  (*exception TypeError of (string * Expr.t list * Type.t option)*)
  (* Assumes that all annotations in a are correct and that the expression is actually well-typed; this function just infers the type from the expression *)
  let rec get_type (ctx : Type.t VariableMap.t) (a : Expr.t) : Type.t =
    match a with
    | Var x -> type_of_var ctx x
    | Let(a1,x,a2) -> 
      let tp1 = get_type ctx a1 in
      get_type (VariableMap.add x tp1 ctx) a2
    | Annot(_,tp) -> tp
    | Zero tp -> tp
    | Plus(a1,_) -> get_type ctx a1
    | Const _ -> Type.Unit
    | Scale(_,a2) -> get_type ctx a2
    | Pair(a1,a2) ->
      let tp1 = get_type ctx a1 in
      let tp2 = get_type ctx a2 in
      Type.Sum(tp1,tp2)
    | Case(a0,x1,a1,_,_) ->
      let tp0 = get_type ctx a0 in
      get_type (VariableMap.add x1 tp0 ctx) a1
    | Lambda(x,tp1,a') ->
      let tp2 = get_type (VariableMap.add x tp1 ctx) a' in
      Type.Arrow(tp1,tp2)
    | Apply(a1,_) ->
      let tp0 = get_type ctx a1 in
      let (_,tp2) = Typing.assert_arrow_type tp0 in
      tp2

  (* TODO: could probably make it so only the typing context is an argument, rather than having both *)
  let rec smtml_of_expr (tps : Type.t VariableMap.t) (ctx : Smtml.Symbol.t VariableMap.t) (a : Expr.t) tp =
    match a with
    | Var x -> var ctx x
    | Let (a1,x,a2) ->
        let tp1 = get_type tps a1 in
        let e1 = smtml_of_expr tps ctx a1 tp1 in
        let s = make_symbol tp1 x in
        let e2 = smtml_of_expr (VariableMap.add x tp1 tps) (VariableMap.add x s ctx) a2 tp in
        Smtml.Expr.let_in [Smtml.Expr.symbol s; e1] e2

    | Zero tp -> zero tp
    | Annot (a, _) -> smtml_of_expr tps ctx a tp
    | Plus (e1, e2) ->
      plus tp (smtml_of_expr tps ctx e1 tp)
              (smtml_of_expr tps ctx e2 tp)
    | Const r -> const r
    | Scale (e1, e2) ->
      scale tp  (smtml_of_expr tps ctx e1 Type.Unit)
                (smtml_of_expr tps ctx e2 tp)
    | Pair (e1, e2) ->
      let (tp1,tp2) = Typing.assert_sum_type tp in
      let e1' = smtml_of_expr tps ctx e1 tp1 in
      let e2' = smtml_of_expr tps ctx e2 tp2 in
      SMT.pair e1' e2'

    (* The annotations here are fairly restrictive, try to make this better using the typechecker/type inference *)
    | Case (e, x1, e1, x2, e2) ->
      let tp0 = get_type tps e in
      let (tp1,tp2) = Typing.assert_sum_type tp0 in
      let s1 = make_symbol tp1 x1 in
      let s2 = make_symbol tp2 x2 in
      let tps1 = VariableMap.add x1 tp1 tps in
      let tps2 = VariableMap.add x2 tp2 tps in
      let ctx1 = VariableMap.add x1 s1 ctx in
      let ctx2 = VariableMap.add x2 s2 ctx in
      let e' = smtml_of_expr tps ctx e (Sum (tp1, tp2)) in
      let e1' = smtml_of_expr tps1 ctx1 e1 tp in
      let e2' = smtml_of_expr tps2 ctx2 e2 tp in
      case tp1 tp2 tp e' s1 e1' s2 e2'

    | Lambda (x,_,e') ->
      let (tp1,tp2) = Typing.assert_arrow_type tp in
      let s = make_symbol tp1 x in
      let tps' = VariableMap.add x tp1 tps in
      let ctx' = VariableMap.add x s ctx in
      let e' = smtml_of_expr tps' ctx' e' tp2 in
      lambda s e'

    (* The annotations here are fairly restrictive, try to make this better *)
    | Apply (e1, e2) ->
      let tp1 = get_type tps e1 in
      let (tp_in,tp_out) = Typing.assert_arrow_type tp1 in
      apply tp_in tp_out (smtml_of_expr tps ctx e1 tp1) (smtml_of_expr tps ctx e2 tp_in)

(*
    | _ -> 
      terr @@ "[smtml_of_expr] Could not convert LambdaC expression to Smtml expression\n"
      ^ "\tExpression: " ^ LambdaC.Expr.pretty_string_of_t a ^ "\n"
      ^ "\tType: " ^ LambdaC.Type.string_of_t tp
*)

  (*
  let rec free_variables (a : Expr.t) : LambdaC.VariableSet.t =
    match a with 
    | Var x -> VariableSet.singleton x
    | Zero _ -> VariableSet.empty (* ?? *)
    | ZeroA (_,ctx) -> ctx
    | Annot (a, _) -> free_variables a
    | Plus (a1,a2) -> free_variables a1 `union` free_variables a2
    | Const r -> VariableSet.empty
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
      terr @@ "[smtml] Cannot coerce smtml value to LambdaC value of function type\n"
        ^ "\tValue: " ^ Smtml.Value.to_string v ^ "\n"
        ^ "\tType: " ^ Type.string_of_t tp ^ "\n"
    | _, _ -> 
      terr @@ "[smtml] Cannot find LambdaC value corresponding to the given smtml value\n"
        ^ "\tValue: " ^ Smtml.Value.to_string v ^ "\n"
        ^ "\tType: " ^ Type.string_of_t tp ^ "\n"

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

  let string_of_inputs (m : Val.t VariableMap.t) : string =
    let f i v s = "x" ^ string_of_int i ^ " |-> " ^ Val.pretty_string_of_t v ^ "; " ^ s in
    "[" ^ VariableMap.fold f m "]"

  let string_of_counterexample counter =
    "\tInput(s): " ^ string_of_inputs counter.inputs
    ^ "\n\t"
    ^ "omega(f i1, f i2) = " ^ Val.pretty_string_of_t counter.lhs ^ "\n\t"
    ^ "omega(i1, i2) = " ^ Val.pretty_string_of_t counter.rhs

  let equiv (tp : Type.t) (ctx : Type.t VariableMap.t) (a1 : Expr.t) (a2 : Expr.t) : (unit, counterexample) result =
    let ctx0 = make_symbol_map ctx in

    let e1 = smtml_of_expr ctx ctx0 a1 tp in
    let e2 = smtml_of_expr ctx ctx0 a2 tp in

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
    | Unknown -> 
      terr @@ "[SmtLambdaC.equiv] Solver timeout checking equivalence:\n"
      ^ "\t1. " ^ Expr.pretty_string_of_t a1 ^ "\n"
      ^ "\t2. " ^ Expr.pretty_string_of_t a2 ^ "\n"
    
end



module SmtLambdaPC (S : SCALARS) = struct
  module SmtC = SmtLambdaC (S.Zd)

  let symplectic_check in_tp out_tp (f : LambdaPC.Expr.pc) : unit =
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
        
        (match SmtC.equiv LambdaC.Type.Unit ctx lhs rhs with
        | Ok _ -> ()
        | Error counter ->
          terr @@ "TYPE ERROR\nSymplectomorphism check failed with the following counterexample:\n" ^ SmtC.string_of_counterexample counter
        )

    let typecheck (pc : LambdaPC.Expr.pc) : Type.t * Type.t =
      let info = LinearityTyping.linearity_check_pc pc in
      let (in_tp,out_tp) = info.tp in
      symplectic_check in_tp out_tp info.expr;
      (in_tp, out_tp)
end
