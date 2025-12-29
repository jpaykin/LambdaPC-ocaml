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

  (* Record the existance of the variable x *)
  let update (x : Variable.t) (env : t) : unit =
    env := max (x+1) !env
end 


module UsageContext = struct
  module M = Set.Make(Variable)
  include M
  (*
  let string_of_t u = List.fold_left (fun str x -> str ^ string_of_int x) "{" (to_list u)  ^ "}"

  let rename from to_ u = M.map (fun z -> if z = from then to_ else z) u
  *)
end

module Expr = struct

  type t =
      Var of Variable.t
    | Let of t * Variable.t * t
    | Zero of Type.t
    | Annot of t * Type.t
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
      | Let (a1,x,a2) -> "Let(" ^ string_of_t a1 ^ ", " ^ string_of_int x ^ ", " ^ string_of_t a2 ^ ")"
      | Zero tp -> "Zero(" ^ Type.string_of_t tp ^ ")"
      | Annot (e, tp) -> "Annot( " ^ string_of_t e ^ ", " ^ Type.string_of_t tp ^ ")"
      | Plus (e1, e2) -> "Plus(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Const c -> "Const(" ^ string_of_int c ^ ")"
      | Scale (e1, e2) -> "Scale(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Pair (e1, e2) -> "Pair(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Case (scrut, x1, e1, x2, e2) ->
          "Case(" ^ string_of_t scrut ^ ", " ^ string_of_int x1 ^ ", " ^ string_of_t e1 ^ ", " ^ string_of_int x2 ^ ", " ^ string_of_t e2 ^ ")"
      | Lambda (x, tp, body) ->
          "Lambda(" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ string_of_t body ^ ")"
      | Apply (e1, e2) -> "Apply(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"

    
    let rec pretty_string_of_t e =
      match e with
      | Var x -> "x" ^ string_of_int x
      | Let (a1,x,a2) -> "let x" ^ string_of_int x ^ " = " ^ pretty_string_of_t a1 ^ " in " ^ pretty_string_of_t a2
      | Zero tp -> "0{" ^ Type.string_of_t tp ^ "}"
      | Annot (e, tp) -> "(" ^ pretty_string_of_t e ^ " : " ^ Type.string_of_t tp ^ ")"
      | Plus (e1, e2) -> pretty_string_of_t e1 ^ " + " ^ pretty_string_of_t e2
      | Const c -> string_of_int c
      | Scale (e1, e2) -> pretty_string_of_t e1 ^ " * " ^ pretty_string_of_t e2
      | Pair (Const 0, Const 0) -> "I"
      | Pair (Const 1, Const 0) -> "X"
      | Pair (Const 0, Const 1) -> "Z"
      | Pair (Const 1, Const 1) -> "Y"
      | Pair (e1, e2) -> "[" ^ pretty_string_of_t e1 ^ ", " ^ pretty_string_of_t e2 ^ "]"
      | Case (scrut, x1, e1, x2, e2) ->
          "case " ^ pretty_string_of_t scrut
          ^ " of { x" ^ string_of_int x1 ^ " -> " ^ pretty_string_of_t e1 
          ^ " | x" ^ string_of_int x2 ^ " -> " ^ pretty_string_of_t e2 ^ "}"
      | Lambda (x, tp, body) ->
          "lambda x" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ pretty_string_of_t body
      | Apply (e1, e2) -> pretty_string_of_t e1 ^ " @ " ^ pretty_string_of_t e2


    let rec rename_var (from : Variable.t) (to_ : Variable.t) (e : t) : t =
      match e with
      | Var x -> if x = from then Var to_ else Var x
      | Let (a1,x,a2) ->
        Let(rename_var from to_ a1,
            x,
            if x = from then a2 else rename_var from to_ a2)
      | Zero tp -> Zero tp
      | Annot (e, tp) -> Annot(rename_var from to_ e, tp)
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

    (* Apply the function f to all constants in e *)
    let rec map (f : int -> int) (e : t) : t =
      match e with
      | Var x -> Var x
      | Let(a1,x,a2) -> Let (map f a1, x, map f a2)
      | Zero tp -> Zero tp
      | Annot (e', tp) -> Annot(map f e', tp)
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

    (* Update env so its next fresh variable is not in e *)
    let rec update_env env e =
      match e with
      | Var x -> VariableEnvironment.update x env
      | Let(a1,x,a2) ->
        update_env env a1;
        VariableEnvironment.update x env;
        update_env env a2
      | Zero _ -> ()
      | Annot (e', _) -> update_env env e'
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
    (* Take as input two expressions. Returns true iff they are syntactically equal (including free variables, possibly not including type or usage annotations) up to renaming their bound variables. To check if two binders are equal e.g. (lambda x1.e1) and (lambda x2.e2), the function will create a fresh variable y from env and rename both x1 and x2 to y.
      Requires: fresh env will always return a variable that does not occur in either e1 or e2
    *)
    let rec alpha_equiv' (env : VariableEnvironment.t) e1 e2 =
      match e1, e2 with
      | Var x1, Var x2 -> x1 = x2
      | Let(a1,x1,a1'), Let(a2,x2,a2') ->
        let x = VariableEnvironment.fresh env in
        alpha_equiv' env a1 a2
          && alpha_equiv' env (rename_var x1 x a1') (rename_var x2 x a2')
      | Zero tp1, Zero tp2 -> tp1 = tp2
      | Annot(e1', tp1), Annot(e2', tp2) -> tp1 = tp2 && alpha_equiv' env e1' e2'
      | Annot(e1', _), _ -> alpha_equiv' env e1' e2
      | _, Annot(e2', _) -> alpha_equiv' env e1 e2'
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

  let update_env (a : Expr.t) = Expr.update_env !var_env a

  let var x = Expr.Var x
  let zero tp = Expr.Zero tp
  let (+) e1 e2 = Expr.Plus (e1,e2)
  let const x = Expr.Const x
  let ( * ) e1 e2 = Expr.Scale (e1,e2)
  let case e fx fz =
      let x = fresh() in
      let z = fresh() in
      Expr.Case(e, x, fx (var x), z, fz (var z))
  
  let lambda tp (f : Expr.t -> Expr.t) =
      let x = fresh() in
      Expr.Lambda (x, tp, f (var x))

  let (@) e1 e2 = Expr.Apply (e1, e2)
  let pair e1 e2 = Expr.Pair (e1,e2)

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


    let rec pretty_string_of_t v =
          match v with
          | Const c -> string_of_int c
          | Pair (Const 0, Const 0) -> "I"
          | Pair (Const 1, Const 0) -> "X"
          | Pair (Const 0, Const 1) -> "Z"
          | Pair (Const 1, Const 1) -> "Y"
          | Pair (v1, v2) -> "(" ^ pretty_string_of_t v1 ^ ", " ^ pretty_string_of_t v2 ^ ")"
          | Lambda (x, tp, e) ->
              "lambda x" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ Expr.pretty_string_of_t e

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
    | Let(a1,x,a2) ->
      let v = eval ctx a1 in
      let ctx' = VariableMap.add x v ctx in
      eval ctx' a2
    | Zero tp -> vzero tp
    | Annot (e', _) -> eval ctx e'
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
          Zd.t_of_int(rz2) * Zd.t_of_int(rx1)
          -
          Zd.t_of_int(rz1) * Zd.t_of_int(rx2)
          )
    | Val.Pair (v11, v12),
      Val.Pair (v21, v22) ->
        Zd.(symplectic_form v11 v21 + symplectic_form v12 v22)
    | _, _ -> failwith "symplectic_form: values must be symplectic"


end

(*
module Typing = struct
  type typing_context = Type.t VariableMap.t
  type usage_context = UsageContext.M.t


  exception TypeError of string
  let terr msg = raise (TypeError msg)
  exception InferenceError

  let type_of_var gamma (x : Variable.t) : Type.t =
    match VariableMap.find_opt x gamma with
    | None ->
      let msg = "Variable " ^ string_of_int x ^ " not found in the typing context" in
      raise (TypeError msg)
    | Some tp -> tp

  (*
  let assert_available x u (expectation : bool) =
    let b = UsageContext.mem x u in
    if b = expectation then ()
    else if expectation then terr @@ "I expected " ^ string_of_int x ^ " to appear in the usage context " ^ UsageContext.string_of_t u ^ "\n"
    else terr @@ "I did not expect " ^ string_of_int x ^ " to appear in the usage context " ^ UsageContext.string_of_t u ^ "\n"

  let assert_type (expected : Type.t option) (actual : Type.t) =
    match expected with
    | Some expected0 -> if expected0 = actual then ()
                        else terr @@ "Expected type: " ^ Type.string_of_t expected0
                                   ^ "\nActual type: " ^ Type.string_of_t actual
    | None -> ()

  let assert_usage (expected : usage_context option) (actual : usage_context) =
    match expected with
    | Some expected0 -> if UsageContext.equal expected0 actual then ()
                        else terr @@ "Expected usage context: " ^ UsageContext.string_of_t expected0
                                   ^ "\nActual usage context: " ^ UsageContext.string_of_t actual
    | None -> ()

  let assert_usage_subset u1 u2 =
    if UsageContext.subset u1 u2 then ()
    else terr @@ "Expected usage context " ^ UsageContext.string_of_t u1
               ^ "\nto be a subset of " ^ UsageContext.string_of_t u2

  let check_var gamma u x (tau0 : Type.t option) (u0 : usage_context option) =
    let tau' = type_of_var gamma x in
    let u' = UsageContext.remove x u in
    assert_available x u true;
    assert_type tau0 tau';
    assert_usage u0 u';
    (tau',u')

  let check_sum (tau0 : Type.t option) : (Type.t option) * (Type.t option) =
    match tau0 with
    | None -> (None, None)
    | Some (Type.Sum (tau1, tau2)) -> (Some tau1, Some tau2)
    | Some tau -> terr @@ "Expected a sum type, received: " ^ Type.string_of_t tau
  let check_arrow (tau0 : Type.t option) : (Type.t option) * (Type.t option) =
    match tau0 with
    | None -> (None, None)
    | Some (Type.Arrow (tau1, tau2)) -> (Some tau1, Some tau2)
    | Some tau -> terr @@ "Expected an arrow type, received: " ^ Type.string_of_t tau

  let rec typecheck0 gamma u e tau0 u0 =
    match e with
    | Expr.Var x -> check_var gamma u x tau0 u0
    | Annot (e',tau) ->
      assert_type tau0 tau;
      typecheck0 gamma u e' (Some tau) u0
    | Zero tau ->
      assert_type tau0 tau;
      (match u0 with
      | Some u' -> (tau, u')
      | None -> raise InferenceError
      )
    | ZeroA (tau, u') ->
      assert_type tau0 tau;
      assert_usage u0 u';
      assert_usage_subset u' u;
      (tau, u')

    | Const _ ->
      assert_type tau0 Type.Unit;
      assert_usage u0 u;
      (Type.Unit, u)


    | Scale (e1, e2) -> (
      try
        let (_, u') = typecheck0 gamma u e1 (Some Type.Unit) None in
        typecheck0 gamma u' e2 tau0 u0
      with
      | InferenceError ->
        let (tau', u') = typecheck0 gamma u e2 tau0 None in
        let (_, u'') = typecheck0 gamma u' e1 (Some Type.Unit) u0 in
        (tau', u'')
    )

    | Plus (e1, e2) -> (
      try
        let (tau,u') = typecheck0 gamma u e1 tau0 u0 in
        typecheck0 gamma u' e2 (Some tau) (Some u')
      with
      | InferenceError ->
        let (tau,u') = typecheck0 gamma u e2 tau0 u0 in
        typecheck0 gamma u' e1 (Some tau) (Some u')
      )

    | Pair (e1, e2) ->
      let (tau10, tau20) = check_sum tau0 in
      (try
        let (tau1, u') = typecheck0 gamma u e1 tau10 u0 in
        let (tau2, _) = typecheck0 gamma u e2 tau20 (Some u') in
        (Type.Sum (tau1,tau2), u')
      with
      | InferenceError ->
        let (tau2, u') = typecheck0 gamma u e2 tau20 u0 in
        let (tau1, _) = typecheck0 gamma u e1 tau10 (Some u') in
        (Type.Sum (tau1,tau2), u') 
      )

    | Case (a, x1, a1, x2, a2) ->
      let (tau, u') = typecheck0 gamma u a None None in
      let (Some tau1, Some tau2) = check_sum (Some tau) [@@warning "-partial-match"] in
      let gamma1 = VariableMap.add x1 tau1 gamma in
      let gamma2 = VariableMap.add x2 tau2 gamma in
      let u1 = UsageContext.add x1 u' in
      let u2 = UsageContext.add x2 u' in
      
      let (tau',u'') =
        (try
          let (tau', u'') = typecheck0 gamma1 u1 a1 tau0 u0 in
          typecheck0 gamma2 u2 a2 (Some tau') (Some u'')
        with
        | InferenceError ->
          let (tau', u'') = typecheck0 gamma2 u2 a2 tau0 u0 in
          typecheck0 gamma1 u1 a1 (Some tau') (Some u'')
        ) in

      assert_available x1 u'' false;
      assert_available x2 u'' false;
      (tau', u'')

    | Lambda (x, tau1, e') ->
      let (tau10, tau20) = check_arrow tau0 in
      assert_type tau10 tau1;
      let gamma' = VariableMap.add x tau1 gamma in
      let u' = UsageContext.add x u in
      let (tau2, u'') = typecheck0 gamma' u' e' tau20 u0 in
      assert_available x u'' false;
      (Type.Arrow (tau1, tau2), u'')

    | Apply (e1, e2) -> (
      try
        let (tau, u') = typecheck0 gamma u e1 None None in
        let (Some tau1, Some tau2) = check_arrow (Some tau) [@@warning "-partial-match"] in
        let (_, u'') = typecheck0 gamma u' e2 (Some tau1) u0 in
        (Type.Arrow(tau1, tau2), u'')
      with
      | InferenceError ->
        let (tau1, u') = typecheck0 gamma u e2 None None in
        let tau0' = Option.map (fun tau2 -> Type.Arrow(tau1,tau2)) tau0 in
        let (tau', u'') = typecheck0 gamma u' e1 tau0' u0 in
        let (Some tau1, Some tau2) = check_arrow (Some tau') [@@warning "-partial-match"] in
        (Type.Arrow (tau1, tau2), u'')
    )

  let typecheck e = 
    let (tau,u) = typecheck0 VariableMap.empty UsageContext.empty e None None in
    if UsageContext.is_empty(u) then tau
    else 
      let msg = "Unused variables: " ^ UsageContext.string_of_t u in
      raise (TypeError msg)
  *)
end

*)