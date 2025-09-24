open Scalars

module Type = struct

  type t =
      | Unit
      | Sum of t * t
      | Arrow of t * t

  let rec string_of_t tp =
          match tp with
          | Unit -> "Unit"
          | Sum (t1, t2) -> "Sum(" ^ string_of_t t1 ^ ", " ^ string_of_t t2 ^ ")"
          | Arrow (t1, t2) -> "Arrow(" ^ string_of_t t1 ^ " -> " ^ string_of_t t2 ^ ")"

end

module Variable = struct
  type t = int
  let compare = Int.compare
end
module VariableMap = Map.Make(Variable)

module VariableEnvironment = struct
  type t = Variable.t ref
  
  let init : t = {contents = 0}
  let fresh (x : t) : Variable.t =
    let y = !x in
    x := y + 1;
    y

  let update (x : Variable.t) (env : t) : unit =
    env := max (x+1) !env
end 


module Expr = struct

  type t =
      Var of Variable.t
    | Zero of Type.t
    | Plus of t * t
    | Const of int
    | Scale of t * t
    | Pair of t * t
    | Case of t * Variable.t * t * Variable.t * t
    | Lambda of Variable.t * Type.t * t
    | Apply of t * t

    let rec string_of_t e =
      match e with
      | Var x -> "Var(" ^ string_of_int x ^ ")"
      | Zero tp -> "Zero(" ^ Type.string_of_t tp ^ ")"
      | Plus (e1, e2) -> "Plus(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Const c -> "Const(" ^ string_of_int c ^ ")"
      | Scale (e1, e2) -> "Scale(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Pair (e1, e2) -> "Pair(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Case (scrut, x1, e1, x2, e2) ->
          "Case(" ^ string_of_t scrut ^ ", " ^ string_of_int x1 ^ ", " ^ string_of_t e1 ^ ", " ^ string_of_int x2 ^ ", " ^ string_of_t e2 ^ ")"
      | Lambda (x, tp, body) ->
          "Lambda(" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ string_of_t body ^ ")"
      | Apply (e1, e2) -> "Apply(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"

    let rec rename_var (from : Variable.t) (to_ : Variable.t) (e : t) : t =
      match e with
      | Var x -> if x = from then Var to_ else Var x
      | Zero tp -> Zero tp
      | Const c -> Const c
      | Plus (e1, e2) -> Plus (rename_var from to_ e1, rename_var from to_ e2)
      | Scale (e1, e2) -> Scale (rename_var from to_ e1, rename_var from to_ e2)
      | Pair (e1, e2) -> Pair (rename_var from to_ e1, rename_var from to_ e2)
      | Case (scrut, x1, e1, x2, e2) ->
          let e1' = if x1 = from then e1 else rename_var from to_ e1 in
          let e2' = if x2 = from then e2 else rename_var from to_ e2 in
          Case (rename_var from to_ scrut, x1, e1', x2, e2')
      | Lambda (x, tp, body) ->
          if x = from then Lambda (x, tp, body)
          else Lambda (x, tp, rename_var from to_ body)
      | Apply (e1, e2) -> Apply (rename_var from to_ e1, rename_var from to_ e2)

    let rec map (f : int -> int) (e : t) : t =
      match e with
      | Var x -> Var x
      | Zero tp -> Zero tp
      | Plus (e1, e2) ->
        Plus (map f e1, map f e2)
      | Const c -> Const (f c)
      | Scale (e1, e2) ->
        Scale (map f e1, map f e2)
      | Pair (e1, e2) ->
        Pair (map f e1, map f e2)
      | Case (e0, x1, e1, x2, e2) ->
        Case (map f e0, x1, map f e1, x2, map f e2)
      | Lambda (x, tp, e') ->
        Lambda (x, tp, map f e')
      | Apply (e1,e2) -> 
        Apply (map f e1, map f e2)

    let rec update_env env e =
      match e with
      | Var x -> VariableEnvironment.update x env
      | Zero _ -> ()
      | Plus (e1, e2) -> update_env env e1; update_env env e2
      | Const _ -> ()
      | Scale (e1, e2) -> update_env env e1; update_env env e2
      | Pair (e1, e2) -> update_env env e1; update_env env e2
      | Case (e', x1, e1', x2, e2') ->
        update_env env e';
        VariableEnvironment.update x1 env;
        VariableEnvironment.update x2 env;
        update_env env e1';
        update_env env e2'
      | Lambda (x,_,e') ->
        VariableEnvironment.update x env;
        update_env env e'
      | Apply (e1, e2) -> update_env env e1; update_env env e2

    (* alpha equivalence *)
    let rec alpha_equiv' (env : VariableEnvironment.t) e1 e2 =
      match e1, e2 with
      | Var x1, Var x2 -> x1 = x2
      | Zero tp1, Zero tp2 -> tp1 = tp2
      | Plus (e11,e12), Plus(e21,e22) -> alpha_equiv' env e11 e21 && alpha_equiv' env e12 e22
      | Const v1, Const v2 -> v1 = v2
      | Scale (e11,e12), Scale (e21,e22) -> alpha_equiv' env e11 e21 && alpha_equiv' env e12 e22
      | Pair (e11,e12), Pair (e21,e22) -> alpha_equiv' env e11 e21 && alpha_equiv' env e12 e22
      | Case (e1',x11,e11,x12,e12), Case(e2',x21,e21,x22,e22) ->
        let x = VariableEnvironment.fresh env in
        alpha_equiv' env e1' e2'
        && alpha_equiv' env (rename_var x11 x e11) (rename_var x21 x e21)
        && alpha_equiv' env (rename_var x12 x e12) (rename_var x22 x e22)
      | Lambda (x1, tp1, e1), Lambda (x2, tp2, e2) ->
        let x = VariableEnvironment.fresh env in
        tp1 = tp2 && alpha_equiv' env (rename_var x1 x e1) (rename_var x2 x e2)
      | Apply (e11,e12), Apply (e21,e22) -> alpha_equiv' env e11 e21 && alpha_equiv' env e12 e22
      | _, _ -> false

    let alpha_equiv e1 e2 =
      let env = VariableEnvironment.init in
      update_env env e1;
      update_env env e2;
      alpha_equiv' env e1 e2
  end

module HOAS = struct
  let var_env : VariableEnvironment.t ref = {contents = VariableEnvironment.init}
  let set_variable_environment (env : VariableEnvironment.t) = var_env := env
  let fresh () : Variable.t = VariableEnvironment.fresh !var_env

   let var x = Expr.Var x
  let zero tp = Expr.Zero tp
  let (+) e1 e2 = Expr.Plus (e1,e2)
  let const x = Expr.Const x
  let ( * ) e1 e2 = Expr.Scale (e1,e2)
  let case e fx fz =
      let x = fresh() in
      let z = fresh() in
      Expr.Case(e, x, fx x, z, fz z)
  
  let lambda tp (f : Variable.t -> Expr.t) =
      let x = fresh() in
      Expr.Lambda (x, tp, f x)
  let (@) e1 e2 = Expr.Apply (e1, e2)
    
end

module Val = struct

    type t =
      | Const of int
      | Pair of t * t
      | Lambda of Variable.t * Type.t * Expr.t

    let rec string_of_t v =
          match v with
          | Const c -> string_of_int c
          | Pair (v1, v2) -> "(" ^ string_of_t v1 ^ ", " ^ string_of_t v2 ^ ")"
          | Lambda (x, tp, e) ->
              "Lambda(" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ Expr.string_of_t e ^ ")"

    let rec expr_of_t v =
      match v with
      | Const c -> Expr.Const c
      | Pair (v1, v2) -> Expr.Pair (expr_of_t v1, expr_of_t v2)
      | Lambda (x, tp, e) -> Expr.Lambda (x, tp, e)


    let rec map (f : int -> int) (v : t) : t =
        match v with
        | Const r -> Const (f r)
        | Pair (v1, v2) -> Pair (map f v1, map f v2)
        | Lambda (x,tp,e) -> Lambda (x,tp, Expr.map f e)

    let alpha_equiv v1 v2 = Expr.alpha_equiv (expr_of_t v1) (expr_of_t v2)

  end

module Eval (Zd : Z_SIG) = struct
  let var_env : VariableEnvironment.t ref = {contents = VariableEnvironment.init}
  let set_variable_environment (env : VariableEnvironment.t) = var_env := env
  let fresh () : Variable.t = VariableEnvironment.fresh !var_env

  let rec vzero (ltp : Type.t) =
    match ltp with
    | Unit -> Val.Const 0
    | Sum (ltp1, ltp2) ->
        let v1 = vzero ltp1 in
        let v2 = vzero ltp2 in
        Val.Pair (v1, v2)
    | Arrow (ltp1, ltp2) ->
        let x = fresh() in
        let v = vzero ltp2 in
        Val.Lambda (x, ltp1, Val.expr_of_t v)


  let rec vplus (v1 : Val.t) (v2 : Val.t) : Val.t =
    match v1, v2 with
    | Val.Const c1, Val.Const c2 -> Val.Const (Zd.normalize(c1 + c2))
    | Val.Pair (a1, b1), Val.Pair (a2, b2) ->
      Val.Pair (vplus a1 a2, vplus b1 b2)
    | Val.Lambda (x1, tp1, e1), Val.Lambda (x2, tp2, e2) ->
        if tp1 = tp2 then
            let fresh_x = fresh() in
            let e1' = Expr.rename_var x1 fresh_x e1 in
            let e2' = Expr.rename_var x2 fresh_x e2 in
            Val.Lambda (fresh_x, tp1, Expr.Plus (e1', e2'))
        else
            failwith "vplus: Lambda types do not match"
    | _, _ -> failwith "vplus: mismatched values"

  let rec vscale (v1 : int) (v2 : Val.t) : Val.t =
    match v2 with
    | Val.Const c -> Val.Const (v1 * c)
    | Val.Pair (a, b) -> Val.Pair (vscale v1 a, vscale v1 b)
    | Val.Lambda (x, tp, e) -> Val.Lambda (x, tp, Scale (Const v1, e))

  let rec eval (ctx : Val.t VariableMap.t) (e : Expr.t) : Val.t =
    match e with
    | Var x -> (try VariableMap.find x ctx with Not_found -> failwith "Unbound variable")
    | Zero tp -> vzero tp
    | Const c -> Val.Const (Zd.normalize c)
    | Plus (e1, e2) -> vplus (eval ctx e1) (eval ctx e2)
    | Scale (e1, e2) ->
        (match eval ctx e1 with
        | Val.Const c -> vscale c (eval ctx e2)
        | _ -> failwith "Scale: first argument must be a scalar")
    | Pair (e1, e2) -> Val.Pair (eval ctx e1, eval ctx e2)
    | Case (scrut, x1, e1, x2, e2) ->
        (match eval ctx scrut with
        | Val.Pair (v1, v2) ->
            let ctx1 = VariableMap.add x1 v1 ctx in
            let ctx2 = VariableMap.add x2 v2 ctx in
            vplus (eval ctx1 e1) (eval ctx2 e2)
        | _ -> failwith "Case: scrutinee must be a pair")
    | Lambda (x, tp, body) -> Val.Lambda (x, tp, body)
    | Apply (e1, e2) ->
        (match eval ctx e1 with
        | Val.Lambda (x, _tp, body) ->
            let arg = eval ctx e2 in
            let ctx' = VariableMap.add x arg ctx in
            eval ctx' body
        | _ -> failwith "Apply: not a lambda")


  let rec symplectic_form (v1 : Val.t) (v2 : Val.t) : Zd.t =
    match v1, v2 with
    | Val.Pair (Val.Const rz1, Val.Const rx1),
      Val.Pair (Val.Const rz2, Val.Const rx2) ->
        Zd.(
          Zd.t_of_int(rz1) * Zd.t_of_int(rx2)
          -
          Zd.t_of_int(rz2) * Zd.t_of_int(rx1)
          )
    | Val.Pair (v11, v12),
      Val.Pair (v21, v22) ->
        Zd.(symplectic_form v11 v21 + symplectic_form v12 v22)
    | _, _ -> failwith "symplectic_form: values must be symplectic"


end

module Typing = struct
  type typing_context = Type.t VariableMap.t

  module Usage = struct
    type t = Z | L | Inf | Unknown

    let (+) (u1 : t) (u2 : t) : t =
      match u1, u2 with
      | Z, u | u, Z -> u
      | Unknown, u | u, Unknown -> u
      | L, L | Inf, _ | _, Inf -> Inf

    let unify u1 u2 : t option =
      match u1, u2 with
      | Unknown, u | u, Unknown -> Some u
      | L, L | Z, Z | Inf, Inf -> Some u1
      | _, _ -> None

  end
  module UsageContext = struct

    type t = Usage.t VariableMap.t

    let (+) = VariableMap.merge (fun _ o1 o2 ->
      match o1, o2 with
      | Some u1, Some u2 -> Some Usage.(u1 + u2)
      | _, _ -> None
      )

    let unify = VariableMap.merge (fun _ o1 o2 ->
      match o1, o2 with
      | Some u1, Some u2 -> Usage.unify u1 u2
      | _, _ -> None
      )

    let is_linear_in x ctx =
      match VariableMap.find_opt x ctx with
      | Some Usage.L | Some Usage.Unknown -> true
      | _ -> false

  end

  exception TypeError of string

  let type_of_var gamma (x : Variable.t) : Type.t * UsageContext.t =
    match VariableMap.find_opt x gamma with
    | None ->
      let msg = "Variable " ^ string_of_int x ^ " not found in the typing context" in
      raise (TypeError msg)
    | Some tp -> (tp, VariableMap.singleton x Usage.L)

  let rec type_of' (gamma : typing_context)
                  (e : Expr.t) : Type.t * UsageContext.t =
    match e with
    | Var x -> type_of_var gamma x
    | Zero tp -> (tp, VariableMap.map (fun _ -> Usage.Unknown) gamma)
    | Plus (e1, e2) ->
      let (tp1, usage1) = type_of' gamma e1 in
      let (tp2, usage2) = type_of' gamma e2 in
      if not(tp1 = tp2)
      then raise (TypeError "Plus: expressions must have the same type")
      else (tp1, UsageContext.unify usage1 usage2)
    | Const _ -> (Unit, VariableMap.map (fun _ -> Usage.Z) gamma)
    | Scale (e1, e2) -> 
      let (tp1, usage1) = type_of' gamma e1 in
      let (tp2, usage2) = type_of' gamma e2 in
      (match tp1 with
       | Unit -> (tp2, UsageContext.(usage1 + usage2))
       | _ -> raise (TypeError "Scale: first argument must have type Unit")
      )
    | Pair (e1, e2) -> 
      let (tp1, usage1) = type_of' gamma e1 in
      let (tp2, usage2) = type_of' gamma e2 in
      (Sum (tp1, tp2), UsageContext.unify usage1 usage2)

    | Case (e0, x1, e1, x2, e2) ->
      (match type_of' gamma e0 with
       | (Sum(tp1,tp2), usage') ->
         let gamma1 = VariableMap.add x1 tp1 gamma in
         let gamma2 = VariableMap.add x2 tp2 gamma in
         let (tp1', usage1) = type_of' gamma1 e1 in
         let (tp2', usage2) = type_of' gamma2 e2 in
         if not (tp1' = tp2')
         then raise (TypeError "Case: branches must have the same type")
         else let usage'' = UsageContext.(usage' + unify usage1 usage2) in
              (tp1', usage'')
       | _ -> raise (TypeError "Case: first argument must have Sum type")
      )
    | Lambda (x, tp, e') -> 
      let gamma' = VariableMap.add x tp gamma in
      let (tp', usage') = type_of' gamma' e' in
      (* check that x is used linearly in e' *)
      if not (UsageContext.is_linear_in x usage')
      then raise (TypeError "Lambda: variable not used linearly in body")
      else let usage0 = VariableMap.remove x usage' in
           (Arrow (tp, tp'), usage0)
    | Apply (e1, e2) ->
      (match type_of' gamma e1, type_of' gamma e2 with
       | (Arrow (tp,tp'), usage1), (tp0, usage2)
        when tp = tp0 -> (tp', UsageContext.(usage1 + usage2))
       | _, _ -> raise (TypeError "Apply: function and argument types do not match")
      )

    let type_of (gamma : typing_context) (e : Expr.t) : Type.t =
      let (tp, usage) = type_of' gamma e in
      let success_criteria x _ = UsageContext.is_linear_in x usage
      in 
      if VariableMap.for_all success_criteria gamma
      then tp
      else raise (TypeError "Non-linear use of variables")
end
