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

  let rec exists_usage_subset (u : t) (f : t -> bool) : bool =
    (* want to check if:
        exists u0, u0 subset u && f u0 }
      if u is empty (u=0), just check if f(0)
      if u is not empty, then pick some x in u:
        u = (u-{x}) U {x}}
      then
        u0 subset u
        <->
        u0 subset (u-{x})
        ||
        (x \in u0 && (u0-{x} subset (u - {x}))

      so 
        exists u0, u0 subset u && f u0
        <->

        exists u0,
          (u0 subset (u-{x}) && f u0)
        ||
        exists u0,
          (x \in u0 && (u0 - {x} subset (u - {x})) && f u0)

        <->

        exists u0,
          (u0 subset (u-{x}) && f u0)
        ||
        exists u0',
          (u0' subset (u - {x})) && f ({x} union u0'))

        <->

        exists u0,
          u0 subset (u - {x})
          && f u0 || f ({x} union u0')
    *)
    if M.is_empty u then f u
    else
      let x = M.choose u in
      exists_usage_subset (M.remove x u)
        (fun u0 -> f u0 || f (M.add x u0))
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

    let rec string_of_t a =
      match a with
      | Var x -> "Var(" ^ string_of_int x ^ ")"
      | Let (a1,x,a2) -> "Let(" ^ string_of_t a1 ^ ", " ^ string_of_int x ^ ", " ^ string_of_t a2 ^ ")"
      | Zero tp -> "Zero(" ^ Type.string_of_t tp ^ ")"
      | Annot (a, tp) -> "Annot( " ^ string_of_t a ^ ", " ^ Type.string_of_t tp ^ ")"
      | Plus (a1, a2) -> "Plus(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Const c -> "Const(" ^ string_of_int c ^ ")"
      | Scale (a1, a2) -> "Scale(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Pair (a1, a2) -> "Pair(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Case (scrut, x1, a1, x2, a2) ->
          "Case(" ^ string_of_t scrut ^ ", " ^ string_of_int x1 ^ ", " ^ string_of_t a1 ^ ", " ^ string_of_int x2 ^ ", " ^ string_of_t a2 ^ ")"
      | Lambda (x, tp, body) ->
          "Lambda(" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ string_of_t body ^ ")"
      | Apply (a1, a2) -> "Apply(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"

    
    let rec pretty_string_of_t a =
      match a with
      | Var x -> "x" ^ string_of_int x
      | Let (a1,x,a2) -> "let x" ^ string_of_int x ^ " = " ^ pretty_string_of_t a1 ^ " in " ^ pretty_string_of_t a2
      | Zero tp -> "0{" ^ Type.string_of_t tp ^ "}"
      | Annot (a, tp) -> "(" ^ pretty_string_of_t a ^ " : " ^ Type.string_of_t tp ^ ")"
      | Plus (a1, a2) -> pretty_string_of_t a1 ^ " + " ^ pretty_string_of_t a2
      | Const c -> string_of_int c
      | Scale (a1, a2) -> pretty_string_of_t a1 ^ " * " ^ pretty_string_of_t a2
      | Pair (Const 0, Const 0) -> "I"
      | Pair (Const 1, Const 0) -> "X"
      | Pair (Const 0, Const 1) -> "Z"
      | Pair (Const 1, Const 1) -> "Y"
      | Pair (a1, a2) -> "[" ^ pretty_string_of_t a1 ^ ", " ^ pretty_string_of_t a2 ^ "]"
      | Case (scrut, x1, a1, x2, a2) ->
          "case " ^ pretty_string_of_t scrut
          ^ " of { x" ^ string_of_int x1 ^ " -> " ^ pretty_string_of_t a1 
          ^ " | x" ^ string_of_int x2 ^ " -> " ^ pretty_string_of_t a2 ^ "}"
      | Lambda (x, tp, body) ->
          "lambda x" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ pretty_string_of_t body
      | Apply (a1, a2) -> pretty_string_of_t a1 ^ " @ " ^ pretty_string_of_t a2


    let rec rename_var (from : Variable.t) (to_ : Variable.t) (a : t) : t =
      match a with
      | Var x -> if x = from then Var to_ else Var x
      | Let (a1,x,a2) ->
        Let(rename_var from to_ a1,
            x,
            if x = from then a2 else rename_var from to_ a2)
      | Zero tp -> Zero tp
      | Annot (a, tp) -> Annot(rename_var from to_ a, tp)
      | Const c -> Const c
      | Plus (a1, a2) -> Plus (rename_var from to_ a1, rename_var from to_ a2)
      | Scale (a1, a2) -> Scale (rename_var from to_ a1, rename_var from to_ a2)
      | Pair (a1, a2) -> Pair (rename_var from to_ a1, rename_var from to_ a2)
      | Case (scrut, x1, a1, x2, a2) ->
          let a1' = if x1 = from then a1 else rename_var from to_ a1 in
          let a2' = if x2 = from then a2 else rename_var from to_ a2 in
          Case (rename_var from to_ scrut, x1, a1', x2, a2')
      | Lambda (x, tp, body) ->
          if x = from then Lambda (x, tp, body)
          else Lambda (x, tp, rename_var from to_ body)
      | Apply (a1, a2) -> Apply (rename_var from to_ a1, rename_var from to_ a2)

    (* Apply the function f to all constants in a *)
    let rec map (f : int -> int) (a : t) : t =
      match a with
      | Var x -> Var x
      | Let(a1,x,a2) -> Let (map f a1, x, map f a2)
      | Zero tp -> Zero tp
      | Annot (a', tp) -> Annot(map f a', tp)
      | Plus (a1, a2) ->
        Plus (map f a1, map f a2)
      | Const c -> Const (f c)
      | Scale (a1, a2) ->
        Scale (map f a1, map f a2)
      | Pair (a1, a2) ->
        Pair (map f a1, map f a2)
      | Case (a0, x1, a1, x2, a2) ->
        Case (map f a0, x1, map f a1, x2, map f a2)
      | Lambda (x, tp, a') ->
        Lambda (x, tp, map f a')
      | Apply (a1,a2) -> 
        Apply (map f a1, map f a2)

    (* Update env so its next fresh variable is not in a *)
    let rec update_env env a =
      match a with
      | Var x -> VariableEnvironment.update x env
      | Let(a1,x,a2) ->
        update_env env a1;
        VariableEnvironment.update x env;
        update_env env a2
      | Zero _ -> ()
      | Annot (a', _) -> update_env env a'
      | Plus (a1, a2) -> update_env env a1; update_env env a2
      | Const _ -> ()
      | Scale (a1, a2) -> update_env env a1; update_env env a2
      | Pair (a1, a2) -> update_env env a1; update_env env a2
      | Case (a', x1, a1', x2, a2') ->
        update_env env a';
        VariableEnvironment.update x1 env;
        VariableEnvironment.update x2 env;
        update_env env a1';
        update_env env a2'
      | Lambda (x,_,a') ->
        VariableEnvironment.update x env;
        update_env env a'
      | Apply (a1, a2) -> update_env env a1; update_env env a2

    (* alpha equivalence *)
    (* Take as input two expressions. Returns true iff they are syntactically equal (including free variables, possibly not including type or usage annotations) up to renaming their bound variables. To check if two binders are equal e.g. (lambda x1.a1) and (lambda x2.a2), the function will create a fresh variable y from env and rename both x1 and x2 to y.
      Requires: fresh env will always return a variable that does not occur in either a1 or a2
    *)
    let rec alpha_equiv' (env : VariableEnvironment.t) a1 a2 =
      match a1, a2 with
      | Var x1, Var x2 -> x1 = x2
      | Let(a1,x1,a1'), Let(a2,x2,a2') ->
        let x = VariableEnvironment.fresh env in
        alpha_equiv' env a1 a2
          && alpha_equiv' env (rename_var x1 x a1') (rename_var x2 x a2')
      | Zero tp1, Zero tp2 -> tp1 = tp2
      | Annot(a1', tp1), Annot(a2', tp2) -> tp1 = tp2 && alpha_equiv' env a1' a2'
      | Annot(a1', _), _ -> alpha_equiv' env a1' a2
      | _, Annot(a2', _) -> alpha_equiv' env a1 a2'
      | Plus (a11,a12), Plus(a21,a22) -> alpha_equiv' env a11 a21 && alpha_equiv' env a12 a22
      | Const v1, Const v2 -> v1 = v2
      | Scale (a11,a12), Scale (a21,a22) -> alpha_equiv' env a11 a21 && alpha_equiv' env a12 a22
      | Pair (a11,a12), Pair (a21,a22) -> alpha_equiv' env a11 a21 && alpha_equiv' env a12 a22
      | Case (a1',x11,a11,x12,a12), Case(a2',x21,a21,x22,a22) ->
        let x = VariableEnvironment.fresh env in
        alpha_equiv' env a1' a2'
        && alpha_equiv' env (rename_var x11 x a11) (rename_var x21 x a21)
        && alpha_equiv' env (rename_var x12 x a12) (rename_var x22 x a22)
      | Lambda (x1, tp1, a1), Lambda (x2, tp2, a2) ->
        let x = VariableEnvironment.fresh env in
        tp1 = tp2 && alpha_equiv' env (rename_var x1 x a1) (rename_var x2 x a2)
      | Apply (a11,a12), Apply (a21,a22) -> alpha_equiv' env a11 a21 && alpha_equiv' env a12 a22
      | _, _ -> false

    let alpha_equiv a1 a2 =
      let env = VariableEnvironment.init in
      update_env env a1;
      update_env env a2;
      alpha_equiv' env a1 a2

  end

module HOAS = struct
  let var_env : VariableEnvironment.t ref = {contents = VariableEnvironment.init}
  let set_variable_environment (env : VariableEnvironment.t) = var_env := env
  let fresh () : Variable.t = VariableEnvironment.fresh !var_env

  let update_env (a : Expr.t) = Expr.update_env !var_env a

  let var x = Expr.Var x
  let zero tp = Expr.Zero tp
  let (+) a1 a2 = Expr.Plus (a1,a2)
  let const x = Expr.Const x
  let ( * ) a1 a2 = Expr.Scale (a1,a2)
  let case a fx fz =
      let x = fresh() in
      let z = fresh() in
      Expr.Case(a, x, fx (var x), z, fz (var z))
  
  let lambda tp (f : Expr.t -> Expr.t) =
      let x = fresh() in
      Expr.Lambda (x, tp, f (var x))

  let (@) a1 a2 = Expr.Apply (a1, a2)
  let pair a1 a2 = Expr.Pair (a1,a2)

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
          | Lambda (x, tp, a) ->
              "Lambda(" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ Expr.string_of_t a ^ ")"


    let rec pretty_string_of_t v =
          match v with
          | Const c -> string_of_int c
          | Pair (Const 0, Const 0) -> "I"
          | Pair (Const 1, Const 0) -> "X"
          | Pair (Const 0, Const 1) -> "Z"
          | Pair (Const 1, Const 1) -> "Y"
          | Pair (v1, v2) -> "(" ^ pretty_string_of_t v1 ^ ", " ^ pretty_string_of_t v2 ^ ")"
          | Lambda (x, tp, a) ->
              "lambda x" ^ string_of_int x ^ ":" ^ Type.string_of_t tp ^ ". " ^ Expr.pretty_string_of_t a

    let rec expr_of_t v =
      match v with
      | Const c -> Expr.Const c
      | Pair (v1, v2) -> Expr.Pair (expr_of_t v1, expr_of_t v2)
      | Lambda (x, tp, a) -> Expr.Lambda (x, tp, a)


    let rec map (f : int -> int) (v : t) : t =
        match v with
        | Const r -> Const (f r)
        | Pair (v1, v2) -> Pair (map f v1, map f v2)
        | Lambda (x,tp,a) -> Lambda (x,tp, Expr.map f a)

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
    | Val.Lambda (x1, tp1, a1), Val.Lambda (x2, tp2, a2) ->
        if tp1 = tp2 then
            let fresh_x = fresh() in
            let a1' = Expr.rename_var x1 fresh_x a1 in
            let a2' = Expr.rename_var x2 fresh_x a2 in
            Val.Lambda (fresh_x, tp1, Expr.Plus (a1', a2'))
        else
            failwith "vplus: Lambda types do not match"
    | _, _ -> failwith "vplus: mismatched values"

  let rec vscale (v1 : int) (v2 : Val.t) : Val.t =
    match v2 with
    | Val.Const c -> Val.Const (v1 * c)
    | Val.Pair (a, b) -> Val.Pair (vscale v1 a, vscale v1 b)
    | Val.Lambda (x, tp, a) -> Val.Lambda (x, tp, Scale (Const v1, a))

  let rec eval (ctx : Val.t VariableMap.t) (a : Expr.t) : Val.t =
    match a with
    | Var x -> (try VariableMap.find x ctx with Not_found -> failwith "Unbound variable")
    | Let(a1,x,a2) ->
      let v = eval ctx a1 in
      let ctx' = VariableMap.add x v ctx in
      eval ctx' a2
    | Zero tp -> vzero tp
    | Annot (a', _) -> eval ctx a'
    | Const c -> Val.Const (Zd.normalize c)
    | Plus (a1, a2) -> vplus (eval ctx a1) (eval ctx a2)
    | Scale (a1, a2) ->
        (match eval ctx a1 with
        | Val.Const c -> vscale c (eval ctx a2)
        | _ -> failwith "Scale: first argument must be a scalar")
    | Pair (a1, a2) -> Val.Pair (eval ctx a1, eval ctx a2)
    | Case (scrut, x1, a1, x2, a2) ->
        (match eval ctx scrut with
        | Val.Pair (v1, v2) ->
            let ctx1 = VariableMap.add x1 v1 ctx in
            let ctx2 = VariableMap.add x2 v2 ctx in
            vplus (eval ctx1 a1) (eval ctx2 a2)
        | _ -> failwith "Case: scrutinee must be a pair")
    | Lambda (x, tp, body) -> Val.Lambda (x, tp, body)
    | Apply (a1, a2) ->
        (match eval ctx a1 with
        | Val.Lambda (x, _tp, body) ->
            let arg = eval ctx a2 in
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


module Typing = struct
  type typing_context = Type.t VariableMap.t
  type usage_relation = UsageContext.M.t -> UsageContext.M.t -> bool


  exception TypeError of string
  let terr msg = raise (TypeError msg)

  let type_of_var gamma (x : Variable.t) : Type.t =
    match VariableMap.find_opt x gamma with
    | None ->
      let msg = "Variable " ^ string_of_int x ^ " not found in the typing context" in
      raise (TypeError msg)
    | Some tp -> tp


  type type_information = {
    usage : usage_relation;
    expr : Expr.t;
    tp : Type.t;
  }

  let assert_arrow_type (alpha : Type.t) : Type.t * Type.t =
    match alpha with
    | Type.Arrow (alpha1, alpha2) -> (alpha1, alpha2)
    | _ -> terr @@ "Expected an arrow type, received: " ^ Type.string_of_t alpha

  let assert_sum_type (alpha : Type.t) : Type.t * Type.t =
    match alpha with
    | Type.Sum (alpha1, alpha2) -> (alpha1, alpha2)
    | _ -> terr @@ "Expected a sum type, received: " ^ Type.string_of_t alpha

  let assert_type (expected : Type.t) (actual : Type.t) =
    if expected = actual then ()
    else terr @@ "Expected type: " ^ Type.string_of_t expected
                                   ^ "\nActual type: " ^ Type.string_of_t actual


  let rec typecheck (ctx : typing_context) (a : Expr.t) : type_information = 
    match a with
    | Var x ->
      {
        expr = Var x;
        tp = type_of_var ctx x;
        usage = fun u1 u2 ->
          UsageContext.mem x u1
          && UsageContext.equal u2 (UsageContext.remove x u1)
      }

    | Let (a1,x,a2) ->
      let info1 = typecheck ctx a1 in
      let ctx' = VariableMap.add x info1.tp ctx in
      let info2 = typecheck ctx' a2 in
      {
        expr = Let(Annot(info1.expr, info1.tp), x, info2.expr);
        tp = info2.tp;
        usage = fun u1 u2 ->
          UsageContext.exists_usage_subset u1 (fun u0 ->
            not (UsageContext.mem x u1)
            && info1.usage u1 u0
            && info2.usage (UsageContext.add x u0) u1
            && not (UsageContext.mem x u2)
            )
      }

    | Zero tp ->
      {
        expr = Zero tp;
        tp = tp;
        usage = fun u1 u2 -> UsageContext.subset u2 u1
      }

    | Annot (a',alpha) ->
      let info' = typecheck ctx a' in
      assert_type alpha info'.tp;
      {
        expr = Annot(info'.expr, alpha);
        tp = alpha;
        usage = info'.usage
      }

    | Plus (a1,a2) ->
      let info1 = typecheck ctx a1 in
      let info2 = typecheck ctx a2 in
      assert_type info1.tp info2.tp;
      {
        expr = Plus (info1.expr, info2.expr);
        tp = info1.tp;
        usage = fun u1 u2 ->
          info1.usage u1 u2 && info2.usage u1 u2
      }

    | Const r ->
      {
        expr = Const r;
        tp = Unit;
        usage = UsageContext.equal
      }

    | Scale (a1,a2) ->
      let info1 = typecheck ctx a1 in
      assert_type Unit info1.tp;
      let info2 = typecheck ctx a2 in
      {
        expr = Scale(info1.expr, info2.expr);
        tp = info2.tp;
        usage = (fun u1 u2 ->
          UsageContext.exists_usage_subset u1 (fun u0 ->
            info1.usage u1 u0
            && info2.usage u0 u2))
      }

    | Pair (a1,a2) ->
      let info1 = typecheck ctx a1 in
      let info2 = typecheck ctx a2 in
      {
        expr = Pair (info1.expr, info2.expr);
        tp = Sum (info1.tp, info2.tp);
        usage = fun u1 u2 ->
          info1.usage u1 u2 && info2.usage u1 u2
      }

    | Case(a',x1,a1,x2,a2) ->
      let info' = typecheck ctx a' in
      let (tp1,tp2) = assert_sum_type info'.tp in
      let ctx1 = VariableMap.add x1 tp1 ctx in
      let ctx2 = VariableMap.add x2 tp2 ctx in

      let info1 = typecheck ctx1 a1 in
      let info2 = typecheck ctx2 a2 in
      assert_type info1.tp info2.tp;

      let usage' u_in u_out u_mid =
        (* variables x1 and x2 should not appear in u_in or u_out at all *)
        UsageContext.is_empty @@ UsageContext.inter
          (UsageContext.union u_in u_out)
          (UsageContext.of_list [x1;x2])
        && info'.usage u_in u_mid
        && info1.usage (UsageContext.add x1 u_mid) u_out
        && info2.usage (UsageContext.add x2 u_mid) u_out
      in
      {
        expr = Case(Annot(info'.expr, info'.tp),x1,info1.expr, x2, info2.expr);
        tp = info1.tp;
        usage = fun u_in u_out ->
          UsageContext.exists_usage_subset u_in (fun u_mid ->
            usage' u_in u_out u_mid
          )
      }

    | Lambda (x,alpha,a') ->
      let info' = typecheck (VariableMap.add x alpha ctx) a' in
      {
        expr = Lambda (x, alpha, info'.expr);
        tp = Arrow(alpha,info'.tp);
        usage = fun u1 u2 ->
          not UsageContext.(mem x (union u1 u2))
          && info'.usage (UsageContext.add x u1) u2
      }

    | Apply (a1,a2) ->
      let info1 = typecheck ctx a1 in
      let info2 = typecheck ctx a2 in
      let (tp1,tp2) = assert_arrow_type info1.tp in
      assert_type tp1 info2.tp;
      {
        expr = Apply (Annot (info1.expr,info1.tp), info2.expr);
        tp = tp2;
        usage = (fun u1 u2 ->
          UsageContext.exists_usage_subset u1 (fun u0 ->
            info1.usage u1 u0
            && info2.usage u0 u2))
      }

end
