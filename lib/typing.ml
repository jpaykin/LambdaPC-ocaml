open Scalars
open LambdaPC

module VariableSet = LambdaC.VariableSet
module TypeInformation = LambdaC.TypeInformation
open TypeInformation

module LinearityTyping = struct
  type type_information = (LambdaPC.Type.t, LambdaPC.Expr.t) LambdaC.TypeInformation.t
  let string_of_info = TypeInformation.string_of_info Type.string_of_t Expr.pretty_string_of_t
  let string_of_pc_info = TypeInformation.string_of_info 
    (fun (tp1,tp2) -> "|" ^ Type.string_of_t tp1 ^ " -o " ^ Type.string_of_t tp2 ^ "|")
    Expr.pretty_string_of_pc
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
      ^ string_of_pc_info info
end

module SMT = struct
  open Smtml

  let pair e1 e2 = Expr.list [e1; e2]
  let fst tp e = Expr.binop tp Ty.Binop.At e (Expr.value (Value.Int 0))
  let snd tp e = Expr.binop tp Ty.Binop.At e (Expr.value (Value.Int 1))
  (*let lambda (x : Symbol.t) (e : Expr.t) = Smtml.Expr.list [Smtml.Expr.symbol x; e]
  *)

  let let_in x e e' =
    (*Expr.let_in [Expr.list [Expr.symbol x;e]] e'*)
    Expr.let_in [Expr.app x [e]] e'
    (*
  let apply in_tp out_tp e1 e2 = 
    (* Assume e1 is a pair corresponding to an smt_lambda, e2 is a value *)
    let x = fst in_tp e1 in
    let e' = snd out_tp e1 in
    let_in x e2 e'
*)

  module Solver = Smtml.Solver.Batch (Smtml.Z3_mappings)
  (*module Solver = Smtml.Solver.Batch (Smtml.Cvc5_mappings)*)
  
  type result =
      Equivalent
    | NotEquivalent of Model.t
    | Unknown

  let is_equiv tp e1 e2 : result =
    let solver = Solver.create () in
    let cond = Expr.unop Ty_bool Not (Expr.relop tp Eq e1 e2) in
    Solver.add solver [ cond ];
    let result = try Solver.check solver [] with
      Failure msg -> terr ("[is_equiv] " ^ msg)
    in
    match result with
    | `Sat ->
        (match Solver.model solver with
        | Some model -> NotEquivalent model
        | None -> failwith "??"
        )
    | `Unsat -> Equivalent
    | `Unknown -> Unknown

end

module SmtLambdaCExpr = struct
  open LambdaC



    (* Assuming that gamma doesn't contain any arrow types,
       eta expand all free variables into Unit type *)
       (*THE below doesn't work because it doesn't account for the same variable appearing in different places:

    let concat = VariableMap.union (fun _ _ _ -> None)

    let rec expand_var env x tp : Type.t VariableMap.t * Expr.t=
      match tp with
      | Type.Unit -> (VariableMap.singleton x tp, Var x)
      | Type.Sum(tp1,tp2) ->
        let x1 = VariableEnvironment.fresh env in
        let x2 = VariableEnvironment.fresh env in
        let (gamma1,a1) = expand_var env x1 tp1 in
        let (gamma2,a2) = expand_var env x2 tp2 in
        (concat gamma1 gamma2, Pair(a1,a2))
      | _ -> terr "Called expand_var on an arrow type"

    let rec eta env gamma a : Type.t VariableMap.t * Expr.t =
      match a with
      | Expr.Var x ->
        expand_var env x (VariableMap.find x gamma)
      | Let(a1,x,a2) ->
        let (gamma1,a1') = eta env gamma a1 in
        let a2' = Expr.subst x a1' a2 in
        let (gamma2,a2'') = eta env gamma a2' in
        (concat gamma1 gamma2, a2'')
      | Zero tp -> (VariableMap.empty, Zero tp)
      | Annot(a',tp) ->
        let (gamma,a'') = eta env gamma a' in
        (gamma,Annot(a'',tp))

      | Plus(a1,a2) ->
        let (gamma1,a1') = eta env gamma a1 in
        let (gamma2,a2') = eta env gamma a2 in
        (concat gamma1 gamma2,)

      | _ -> terr @ "Called eta on an unsupported constructor"

    *)
    let rec eta_expand_var env x tp (gamma0,e0) : Type.t VariableMap.t * Expr.t =
      match tp with
      | Type.Unit -> (VariableMap.add x tp gamma0, e0)
      | Type.Sum(tp1,tp2) ->
          let x1 = VariableEnvironment.fresh env in
          let x2 = VariableEnvironment.fresh env in
          let e0' = Expr.subst x (Pair (Var x1, Var x2)) e0 in
          let (gamma1,e1') = eta_expand_var env x1 tp1 (gamma0,e0') in
          let (gamma2,e2') = eta_expand_var env x2 tp2 (gamma1,e1') in
          (gamma2, e2')
      | _ -> terr "Called eta_expand_var on function type"

    let string_of_typing_context gamma =
      "[" ^ VariableMap.fold (fun x tp s -> string_of_int x ^ " : " ^ Type.string_of_t tp ^ ", " ^ s) gamma "]\n"

    [@@@warning "-32"]
    let eta gamma a : Type.t VariableMap.t * Expr.t =
      
      debug @@  "Eta expanding: " ^ Expr.pretty_string_of_t a ^ "\n";
      HOAS.update_env a;
      let (gamma',a') = VariableMap.fold (eta_expand_var !HOAS.var_env)
        gamma (VariableMap.empty, a) in
      debug @@  "Got eta expanded expression: " ^ Expr.pretty_string_of_t a' ^ "\n";
      debug @@ "Got new type context: " ^ string_of_typing_context gamma';
      (gamma',a')



      (* not worrying about linearity,
         normalize into the following form:
      [normal forms]
      v := r | Lambda(x,v) | (v1,v2) | n1 + n2 | v * n2 | n
      [neutral terms]
      n := x | n v | case n of {in1 x1 -> v1 | in2 x2 -> v2}
        | v * n
    *)
    type normal =
      NConst of int
    | NLambda of Variable.t * Type.t * normal
    | NPair of normal * normal
    | Annot of neutral * Type.t
    | Neutral of neutral
    and neutral =
      NVar of Variable.t
    | NApply of neutral * normal
    | NCase of neutral * Variable.t * normal * Variable.t * normal
    | NPlus of neutral * normal
    | NScale of normal * neutral

    let rec nzero env tp = 
      match tp with
      | LambdaC.Type.Unit -> NConst 0
      | Sum(tp1,tp2) -> NPair(nzero env tp1, nzero env tp2)
      | Arrow(tp1,tp2) ->
        let x = VariableEnvironment.fresh env in
        NLambda(x,tp1,nzero env tp2)

    
    let rec nscale e1 e2 =
      match e1, e2 with
      | NConst r1, NConst r2 -> NConst (r1 * r2)
      | _, NConst 0 -> NConst 0
      | _, NConst 1 -> e1
      | Neutral e1', NConst _ -> Neutral (NScale(e2,e1'))
      | Annot(e1',_), NConst _ -> Neutral (NScale(e2,e1'))
      | _, NLambda(x, tp, e2') -> NLambda(x,tp,nscale e1 e2')
      | _, NPair(e2',e2'') -> NPair(nscale e1 e2', nscale e1 e2'')
      | _, Neutral e2' -> Neutral (NScale(e1,e2'))
      | _, Annot(e2',tp) -> Annot (NScale(e1,e2'), tp)
      | _, _ -> failwith "[Typing.SmtLambdaC.Expr.nscale] type mismatch"

    let rec annot e tp =
      match e,tp with
      | NLambda(x,_,e'),Type.Arrow(tp1,tp2) -> NLambda(x,tp1,annot e' tp2)
      | NPair(e1,e2),Type.Sum(tp1,tp2) -> NPair(annot e1 tp1, annot e2 tp2)
      | Neutral e', _ -> Annot(e',tp)
      | Annot(e',_),_ -> Annot(e',tp)
      | _, _ -> failwith "[Typing.SmtLambdaC.Expr.annot] type mismatch"

    let rec subst env from to_ e =
      match e with
      | NConst r -> NConst r
      | NLambda(x,tp,e') ->
        if x=from then NLambda(x,tp,e') else NLambda(x,tp,subst env from to_ e')
      | NPair(e1,e2) -> NPair(subst env from to_ e1, subst env from to_ e2)
      | Neutral e' -> substN env from to_ e'
      | Annot(e',tp) -> annot (substN env from to_ e') tp
    and substN env from to_ e =
      match e with
      | NVar x -> if x=from then to_ else Neutral (NVar x)
      | NApply(e1,e2) -> napply env (substN env from to_ e1) (subst env from to_ e2) 
      | NCase(e',x1,e1,x2,e2) ->
        let e1' = if x1=from then e1 else subst env from to_ e1 in
        let e2' = if x2=from then e2 else subst env from to_ e2 in
        ncase env (substN env from to_ e') x1 e1' x2 e2'
      | NPlus(e1,e2) -> nplus env (substN env from to_ e1) (subst env from to_ e2)
      | NScale(e1,e2) -> nscale (subst env from to_ e1) (substN env from to_ e2)


    and napply env e1 e2 =
      match e1 with
      | NLambda(x,_,e1') -> subst env x e2 e1'
      | Neutral e1' -> Neutral (NApply(e1',e2))
      | Annot(e1',Arrow(_,tp2)) -> Annot (NApply(e1',e2), tp2)
      | _ -> failwith "[Typing.SmtLambdaC.Expr.napply] type mismatch"

    and nplus env e1 e2 =
      match e1, e2 with
      | NConst r1, NConst r2 -> NConst (r1 + r2)
      | NLambda(x1,tp1,e1'), NLambda(x2,_,e2') ->
        let x = VariableEnvironment.fresh env in
        NLambda(x,tp1,
          nplus env (subst env x1 (Neutral (NVar x)) e1')
                    (subst env x2 (Neutral (NVar x)) e2'))
      | NPair (e1',e1''), NPair(e2',e2'') ->
        NPair(nplus env e1' e2', nplus env e1'' e2'')
      | Neutral e1', _ -> Neutral (NPlus(e1',e2))
      | Annot(e1',tp), _ -> Annot(NPlus(e1',e2), tp)
      | _, Neutral e2' -> Neutral (NPlus(e2', e1))
      | _, _ -> failwith "[Typing.SmtLambdaC.Expr.nplus] type mismatch"

    and ncase env e x1 e1 x2 e2 =
      match e with
      | NPair(e1',e2') ->
        nplus env (subst env x1 e1' e1) (subst env x2 e2' e2)
      | Neutral e' ->
        Neutral (NCase(e',x1,e1,x2,e2))
      | Annot(e',_) ->
        Neutral (NCase(e',x1,e1,x2,e2))
      | _ -> failwith "[Typing.SmtLambdaC.Expr.ncase] type mismatch"

    let rec normalize' env (a : Expr.t) =
      match a with
      | Expr.Var x -> Neutral (NVar x)
      | Let(a1,x,a2) ->
        subst env x (normalize' env a1) (normalize' env a2)
      | Zero tp -> nzero env tp
      | Annot(a',_) -> normalize' env a'
      | Plus(a1,a2) -> nplus env (normalize' env a1) (normalize' env a2)
      | Const r -> NConst r
      | Scale(a1,a2) -> nscale (normalize' env a1) (normalize' env a2)
      | Pair(a1,a2) -> NPair(normalize' env a1, normalize' env a2)
      | Case(a',x1,a1,x2,a2) ->
        ncase env (normalize' env a')
              x1  (normalize' env a1)
              x2  (normalize' env a2)
      | Lambda(x,tp,a') -> NLambda(x,tp,normalize' env a')
      | Apply(a1,a2) -> napply env (normalize' env a1) (normalize' env a2)

    let rec expr_of_normal (e : normal) : Expr.t =
      match e with
      | NConst r -> Const r
      | NLambda(x,tp,e') -> Lambda(x,tp,expr_of_normal e')
      | NPair(e1,e2) -> Pair(expr_of_normal e1, expr_of_normal e2)
      | Neutral e' -> expr_of_neutral e'
      | Annot(e',tp) -> Annot(expr_of_neutral e', tp)
    and expr_of_neutral (e : neutral) : Expr.t =
      match e with
      | NVar x -> Var x
      | NApply(e1,e2) -> Apply(expr_of_neutral e1, expr_of_normal e2)
      | NCase(e0,x1,e1,x2,e2) -> Case(expr_of_neutral e0,x1,expr_of_normal e1,x2,expr_of_normal e2)
      | NPlus(e1,e2) -> Plus(expr_of_neutral e1, expr_of_normal e2)
      | NScale(e1,e2) -> Scale(expr_of_normal e1, expr_of_neutral e2)

    let normalize a =
      debug @@  "Normalizing: " ^ Expr.pretty_string_of_t a ^ "\n";
      HOAS.update_env a;
      let e = normalize' (!HOAS.var_env) a in
      let a' = expr_of_normal e in
      debug @@  "Got normalized expression: " ^ Expr.pretty_string_of_t a' ^ "\n";
      a'

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

  (*
  let lambda x e = SMT.lambda x e
  let apply tp1 tp2 e1 e2 = SMT.apply (smtml_of_type tp1) (smtml_of_type tp2) e1 e2
  *)

  (* typed symbols are for free variables *)
  let make_typed_symbol tp x =
    Smtml.Symbol.make (smtml_of_type tp) ("x" ^ string_of_int x)
  (* untyped symbols are for bound variables *)
  let make_untyped_symbol x =
    Smtml.Symbol.make Smtml.Ty.Ty_none ("x" ^ string_of_int x)
  (*
  [@@@warning "-32"]
  let fresh_symbol tp = 
    let x = LambdaC.HOAS.fresh () in
    make_symbol tp x
    *)

  let rec zero (tp : Type.t) : Smtml.Expr.t =
    match tp with
    | Unit -> const 0
    | Sum (tp1, tp2) -> SMT.pair (zero tp1) (zero tp2)
    (*| Arrow(tp1, tp2) -> lambda (fresh_symbol tp1) (zero tp2)*)
    | _ -> terr @@ "SmtLambdaC has assumed no arrow types\n"

  let fst tp1 e = SMT.fst (smtml_of_type tp1) e
  let snd tp2 e = SMT.snd (smtml_of_type tp2) e

  let rec plus (tp : Type.t) e1 e2 =
    match tp with
    | Unit -> e1 + e2
    | Sum(tp1, tp2) ->
        let e1' = plus tp1 (fst tp1 e1) (fst tp1 e2) in
        let e2' = plus tp2 (snd tp2 e1) (snd tp2 e2) in
        (SMT.pair e1' e2')
    (*
    | Arrow(tp1, tp2) ->
        let x = fresh_symbol tp1 in
        lambda x 
          (plus tp2 (apply tp1 tp2 e1 (Smtml.Expr.symbol x))
                    (apply tp1 tp2 e2 (Smtml.Expr.symbol x)))
                    *)
    | _ -> terr @@ "SmtLambdaC has assumed no arrow types"

  let rec scale (tp : Type.t) e e' =
    match tp with
    | Unit -> e * e'
    | Sum(tp1, tp2) ->
      let e1' = scale tp1 e (fst tp1 e') in
      let e2' = scale tp2 e (snd tp2 e') in
      SMT.pair e1' e2'
    | _ -> terr @@ "SmtLambdaC has assumed no arrow types"
(*
    | Arrow(tp1, tp2) ->
      let x = fresh_symbol tp1 in
      lambda x (scale tp2 e (apply tp1 tp2 e' (Smtml.Expr.symbol x)))
      *)

  let case tp1 tp2 tp' e x1 e1 x2 e2 =
    let v1 = fst tp1 e in
    let v2 = snd tp2 e in
    let e1' = SMT.let_in x1 v1 e1 in
    let e2' = SMT.let_in x2 v2 e2 in
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
      let (tp1,_) = Typing.assert_sum_type tp0 in
      get_type (VariableMap.add x1 tp1 ctx) a1
      
    | Lambda(x,tp1,a') ->
      let tp2 = get_type (VariableMap.add x tp1 ctx) a' in
      Type.Arrow(tp1,tp2)
    | Apply(a1,_) ->
      let tp0 = get_type ctx a1 in
      let (_,tp2) = Typing.assert_arrow_type tp0 in
      tp2

  (* TODO: could probably make it so only the typing context is an argument, rather than having both *)
  (* Assume that a has been normalized and neither tps nor tp contain any occurrances of the Arrow type. In that case, a should not contain any instances of Zero, Let, Apply or Lambda
  *)
  let rec smtml_of_expr (tps : Type.t VariableMap.t) (ctx : Smtml.Symbol.t VariableMap.t) (a : Expr.t) tp =
    match a with
    | Var x -> var ctx x
    (*
    | Let (a1,x,a2) ->
        let tp1 = get_type tps a1 in
        let e1 = smtml_of_expr tps ctx a1 tp1 in
        let s = make_symbol tp1 x in
        let e2 = smtml_of_expr (VariableMap.add x tp1 tps) (VariableMap.add x s ctx) a2 tp in
        SMT.let_in (Smtml.Expr.symbol s) e1 e2
    *)

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

    | Case (e, x1, e1, x2, e2) ->
      let tp0 = get_type tps e in
      let (tp1,tp2) = Typing.assert_sum_type tp0 in
      let s1 = make_untyped_symbol x1 in
      let s2 = make_untyped_symbol x2 in
      let tps1 = VariableMap.add x1 tp1 tps in
      let tps2 = VariableMap.add x2 tp2 tps in
      let ctx1 = VariableMap.add x1 s1 ctx in
      let ctx2 = VariableMap.add x2 s2 ctx in
      let e' = smtml_of_expr tps ctx e (Sum (tp1, tp2)) in
      let e1' = smtml_of_expr tps1 ctx1 e1 tp in
      let e2' = smtml_of_expr tps2 ctx2 e2 tp in
      case tp1 tp2 tp e' s1 e1' s2 e2'

    (*
    | Lambda (x,_,e') ->
      let (tp1,tp2) = Typing.assert_arrow_type tp in
      let s = make_symbol tp1 x in
      let tps' = VariableMap.add x tp1 tps in
      let ctx' = VariableMap.add x s ctx in
      let e' = smtml_of_expr tps' ctx' e' tp2 in
      lambda s e'

    | Apply (Lambda (x,tp,body), e2) ->
      (* If the function being applied is a syntactic lambda, we can emit a proper let-binding
         that uses the lambda's symbol directly. This avoids attempting to use a computed
         expression as a binder (which Smtml cannot encode). *)
      let (tp_in, tp_out) = Typing.assert_arrow_type (get_type tps (Lambda (x,tp,body))) in
      let s = make_symbol tp_in x in
      let e2' = smtml_of_expr tps ctx e2 tp_in in
      let body' = smtml_of_expr (VariableMap.add x tp_in tps) (VariableMap.add x s ctx) body tp_out in
      Smtml.Expr.let_in [Smtml.Expr.list [Smtml.Expr.symbol s; e2']] body'

    | Apply (e1, _) ->
      (* We cannot, in general, extract a binder symbol from a dynamically computed lambda; refuse to encode. *)
      terr @@ "[smtml_of_expr] Cannot encode application of dynamic/non-literal lambda: " ^ Expr.pretty_string_of_t e1
    *)

    | _ -> 
      terr @@ "[smtml_of_expr] Could not convert LambdaC expression to Smtml expression\n"
      ^ "\tExpression: " ^ LambdaC.Expr.pretty_string_of_t a ^ "\n"
      ^ "\tType: " ^ LambdaC.Type.string_of_t tp

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
    let f x tp = make_typed_symbol tp x in
    VariableMap.mapi f ctx


  let instantiate model (x : Variable.t) (tp : Type.t) : Val.t =
    let s = make_typed_symbol tp x in
    match Smtml.Model.evaluate model s with
    | Some v0 -> value_of_smtml tp v0
    | None -> terr @@ "Could not instantiate model of variable x" ^ string_of_int x ^ "\n"

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

    let (ctx',a1',a2') =
      (match SmtLambdaCExpr.eta ctx (Pair(a1,a2)) with
      | (ctx,Pair(a1',a2')) -> (ctx,a1',a2')
      | _ -> terr @@ "eta did something wrong"
      ) in

    let a1'' = SmtLambdaCExpr.normalize a1' in
    let a2'' = SmtLambdaCExpr.normalize a2' in

    let ctx0 = make_symbol_map ctx' in
    let e1 = smtml_of_expr ctx' ctx0 a1'' tp in
    let e2 = smtml_of_expr ctx' ctx0 a2'' tp in
    debug "[equiv]\n";
    debug @@ "e1: " ^ Smtml.Expr.to_string e1 ^ "\n";
    debug @@ "e2: " ^ Smtml.Expr.to_string e2 ^ "\n";

    
    let e1 = Smtml.Rewrite.(rewrite_expr (Symb_map.empty, Symb_map.empty) e1) in
    let e2 = Smtml.Rewrite.(rewrite_expr (Symb_map.empty, Symb_map.empty) e2) in


    debug "[equiv,after rewrite]\n";
    debug @@ "e1: " ^ Smtml.Expr.to_string e1 ^ "\n";
    debug @@ "e2: " ^ Smtml.Expr.to_string e2 ^ "\n";


    match SMT.is_equiv (smtml_of_type tp) e1 e2 with
    | Equivalent ->
      debug "IS EQUIVALENT\n";
      Ok ()
    | NotEquivalent model ->
      (* TODO: this counterexample finder is no longer working; need to combine it with the eta expansion idea above. *)
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
    let (Lam(x,_,t)) = f in
        let in_tp' = LambdaPC.Type.ltype_of_t in_tp in

        (* create the expression lhs = omega(psiof(t)[x1/x],psiof(t)[x2/x])*)
        let a = LambdaPC.SymplecticForm.psi_of t in
        debug @@ "Got LambdaC expression: " ^ LambdaC.Expr.pretty_string_of_t a ^ "\n";
        LambdaC.HOAS.update_env a;
        let x1 = LambdaC.HOAS.fresh () in
        let x2 = LambdaC.HOAS.fresh () in
        let a1 = LambdaC.Expr.rename_var x x1 a in
        let a2 = LambdaC.Expr.rename_var x x2 a in
        let lhs = LambdaPC.SymplecticForm.omega out_tp a1 a2 in
        debug @@ "LHS: " ^ LambdaC.Expr.pretty_string_of_t lhs ^ "\n";

        (* create the expression rhs = omega(x1,x2) *)
        let rhs = LambdaPC.SymplecticForm.omega in_tp (LambdaC.HOAS.var x1) (LambdaC.HOAS.var x2) in
        debug @@ "RHS: " ^ LambdaC.Expr.pretty_string_of_t rhs ^ "\n";

        (* check for equivalence *)
        let ctx = LambdaC.VariableMap.of_list [(x1,in_tp'); (x2,in_tp')] in
        match SmtC.equiv LambdaC.Type.Unit ctx lhs rhs with
        | Ok _ -> ()
        | Error counter ->
          terr @@ "TYPE ERROR\nSymplectomorphism check failed with the following counterexample:\n" ^ SmtC.string_of_counterexample counter

  let typecheck (pc : LambdaPC.Expr.pc) : Type.t * Type.t =
      let info = LinearityTyping.linearity_check_pc pc in
      print_string @@ "Passed linearity check\n";
      let (in_tp,out_tp) = info.tp in
      symplectic_check in_tp out_tp info.expr;
      print_string @@ "Passed symplectomorphism check\n";
      print_string @@ LinearityTyping.string_of_pc_info info;
      print_string "\n\n";
      (in_tp, out_tp)
end
