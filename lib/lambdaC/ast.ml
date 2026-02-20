open Core.Scalars
open Interface.Ident

module Type = struct
    type t = { loc : Loc.t; node : node }
    and node =
      | Unit
      | Sum of t * t
      | Arrow of t * t

  (* TODO: add a version that also prints out debug information *)
  let rec string_of_t tp =
          match tp.node with
          | Unit -> "Unit"
          | Sum (t1, t2) -> "Sum(" ^ string_of_t t1 ^ ", " ^ string_of_t t2 ^ ")"
          | Arrow (t1, t2) -> "Arrow(" ^ string_of_t t1 ^ " -> " ^ string_of_t t2 ^ ")"
end

module Expr = struct
  type t = { loc : Loc.t; ty : Type.t option; node : node }
  and node =
    | Var of Ident.t
    | Zero of Type.t
    | Const of int
    | Plus of t * t
    | Scale of t * t
    | Pair of t * t
    | Case of
        { scrut : t
        ; x1 : Ident.t
        ; a1 : t
        ; x2 : Ident.t
        ; a2 : t
        }
    | Lambda of { x : Ident.t; tp : Type.t; body : t }
    | App of t * t
    | Let of { x : Ident.t; lhs : t; body : t }
    | Annot of t * Type.t

    (* TODO: write a version that also outputs debugging information *)
    let rec string_of_t a =
      match a.node with
      | Var x -> "Var(" ^ Ident.string_of_t x ^ ")"
      | Let {x = x; lhs = a1; body = a2} -> "Let(" ^ string_of_t a1 ^ ", " ^ Ident.string_of_t x ^ ", " ^ string_of_t a2 ^ ")"
      | Zero tp -> "Zero(" ^ Type.string_of_t tp ^ ")"
      | Annot (a, tp) -> "Annot( " ^ string_of_t a ^ ", " ^ Type.string_of_t tp ^ ")"
      | Plus (a1, a2) -> "Plus(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Const c -> "Const(" ^ string_of_int c ^ ")"
      | Scale (a1, a2) -> "Scale(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Pair (a1, a2) -> "Pair(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Case {scrut; x1; a1; x2; a2} ->
          "Case(" ^ string_of_t scrut ^ ", " ^ Ident.string_of_t x1 ^ ", " ^ string_of_t a1 ^ ", " ^ Ident.string_of_t x2 ^ ", " ^ string_of_t a2 ^ ")"
      | Lambda {x; tp; body} ->
          "Lambda(" ^ Ident.string_of_t x ^ ":" ^ Type.string_of_t tp ^ ". " ^ string_of_t body ^ ")"
      | App (a1, a2) -> "Apply(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"

    let update_node (e : t) (n : node) =
      { loc = e.loc;
        ty = e.ty;
        node = n
      }
    
    let rec pretty_string_of_t (a : t) =
      match a.node with
      | Var x -> Ident.string_of_t x
      | Let { x ; lhs ; body } ->
        "let x" ^ Ident.string_of_t x
        ^ " = " ^ pretty_string_of_t lhs
        ^ " in " ^ pretty_string_of_t body
      | Zero tp -> "0{" ^ Type.string_of_t tp ^ "}"
      | Annot (a, tp) -> "(" ^ pretty_string_of_t a ^ " : " ^ Type.string_of_t tp ^ ")"
      | Plus (a1, a2) -> pretty_string_of_t a1 ^ " + " ^ pretty_string_of_t a2
      | Const c -> string_of_int c
      | Scale (a1, a2) -> pretty_string_of_t a1 ^ " * " ^ pretty_string_of_t a2
      (*
      | Pair (Const 0, Const 0) -> "I"
      | Pair (Const 1, Const 0) -> "X"
      | Pair (Const 0, Const 1) -> "Z"
      | Pair (Const 1, Const 1) -> "Y"
      *)
      | Pair (a1, a2) -> "[" ^ pretty_string_of_t a1 ^ ", " ^ pretty_string_of_t a2 ^ "]"
      | Case {scrut; x1; a1; x2; a2} ->
          "case " ^ pretty_string_of_t scrut
          ^ " of { x" ^ Ident.string_of_t x1 ^ " -> " ^ pretty_string_of_t a1 
          ^ " | x" ^ Ident.string_of_t x2 ^ " -> " ^ pretty_string_of_t a2 ^ "}"
      | Lambda {x; tp; body} ->
          "lambda x" ^ Ident.string_of_t x ^ ":" ^ Type.string_of_t tp ^ ". " ^ pretty_string_of_t body
      | App (a1, a2) -> pretty_string_of_t a1 ^ " @ " ^ pretty_string_of_t a2


    (* NOT epxlicitly capture-avoiding, assumes NO reuse of variables *)
    let rec subst (from : Ident.t) (to_ : t) (a : t) : t =
      match a.node with
      | Var x -> if Ident.equal x from then to_ else a
      | Let {x; lhs; body} ->
        update_node a @@ Let {
              x = x;
              lhs = subst from to_ lhs;
              body = if Ident.equal x from then body
                     else subst from to_ body
        }
      | Zero _ -> a
      | Annot (a, tp) -> update_node a @@ Annot(subst from to_ a, tp)
      | Const _ -> a
      | Plus (a1, a2) -> update_node a @@
        Plus (subst from to_ a1, subst from to_ a2)
      | Scale (a1, a2) -> update_node a @@
        Scale (subst from to_ a1, subst from to_ a2)
      | Pair (a1, a2) -> update_node a @@
        Pair (subst from to_ a1, subst from to_ a2)
      | Case {scrut; x1; a1; x2; a2} -> update_node a @@
          let a1' = if Ident.equal x1 from then a1
                    else subst from to_ a1 in
          let a2' = if Ident.equal x2 from then a2
                    else subst from to_ a2 in
          Case {scrut = subst from to_ scrut;
                x1;
                a1=a1';
                x2;
                a2=a2'}
      | Lambda {x; tp; body} -> update_node a @@
         let body' = if Ident.equal x from then body
                     else subst from to_ body in
          Lambda {x; tp; body=body'}
      | App (a1, a2) -> update_node a @@
        App (subst from to_ a1, subst from to_ a2)
    
    let var (x : Ident.t) : t =
      {loc = x.loc; ty = None; node = Var x}
    let rename (x : Ident.t) (y : Ident.t) e = 
      subst x (var y) e

    (*App the function f to all constants in a *)
    let rec map (f : int -> int) (a : t) : t =
      match a.node with
      | Var _ -> a
      | Let{x;lhs;body} -> update_node a @@ Let {
          x;
          lhs = map f lhs;
          body = map f body
        }
      | Zero _ -> a
      | Annot (a', tp) -> update_node a @@
        Annot(map f a', tp)
      | Plus (a1, a2) -> update_node a @@
        Plus (map f a1, map f a2)
      | Const c -> update_node a @@ Const (f c)
      | Scale (a1, a2) -> update_node a @@
        Scale (map f a1, map f a2)
      | Pair (a1, a2) -> update_node a @@
        Pair (map f a1, map f a2)
      | Case {scrut; x1; a1; x2; a2} -> update_node a @@
        Case {
          scrut = map f scrut;
          x1;
          a1=map f a1;
          x2;
          a2=map f a2
        }
      | Lambda {x; tp; body} -> update_node a @@
        Lambda {x; tp; body= map f body}
      | App (a1,a2) -> update_node a @@
        App (map f a1, map f a2)

    (* Update env so its next fresh variable is not in a *)
    (* TODO: Joe should recreate this functionality to enable alpha equivalence etc *)
    let update_env env a = a
    (*
    let rec update_env env a =
      match a with
      | Var x -> VariableEnvironment.update x env
      | Let (a1,x,a2) ->
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
      | App (a1, a2) -> update_env env a1; update_env env a2
    *)

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
      |App (a11,a12),App (a21,a22) -> alpha_equiv' env a11 a21 && alpha_equiv' env a12 a22
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
              "Lambda(" ^ Ident.string_of_t x ^ ":" ^ Type.string_of_t tp ^ ". " ^ Expr.string_of_t a ^ ")"


    let rec pretty_string_of_t v =
          match v with
          | Const c -> Ident.string_of_t c
          | Pair (Const 0, Const 0) -> "I"
          | Pair (Const 1, Const 0) -> "X"
          | Pair (Const 0, Const 1) -> "Z"
          | Pair (Const 1, Const 1) -> "Y"
          | Pair (v1, v2) -> "(" ^ pretty_string_of_t v1 ^ ", " ^ pretty_string_of_t v2 ^ ")"
          | Lambda (x, tp, a) ->
              "lambda x" ^ Ident.string_of_t x ^ ":" ^ Type.string_of_t tp ^ ". " ^ Expr.pretty_string_of_t a

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
    |App (a1, a2) ->
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

module TypeInformation = struct
  type usage_relation = VariableSet.t -> VariableSet.t -> bool

  type ('tp, 'expr) t = {
    usage : usage_relation;
    expr : 'expr;
    tp : 'tp;
  }

  let string_of_info string_of_tp string_of_expr info =
    "Type Information:\n"
    ^ "\tAnnotated Expression: " ^ string_of_expr info.expr ^"\n"
    ^ "\tType: " ^ string_of_tp info.tp ^"\n"
  let pp_info string_of_tp string_of_expr info =
    print_endline (string_of_info string_of_tp string_of_expr info);
    flush stdout


  exception TypeError
  let terr msg =
    print_string "TYPE ERROR\n";
    print_endline msg;
    flush stdout;
    raise TypeError
  let debug _msg =
    (*print_string _msg;*)
    ()

  let type_of_var (gamma : 'a VariableMap.t) (x : Variable.t) : 'a =
    match VariableMap.find_opt x gamma with
    | None ->
      let msg = "Variable " ^ Ident.string_of_t x ^ " not found in the typing context" in
      terr msg
    | Some tp -> tp

  
  let assert_type string_of_a (expected : 'a) (actual : 'a) =
    if expected = actual then ()
    else terr @@ "Expected type: " ^ string_of_a expected
                                   ^ "\nActual type: " ^ string_of_a actual

  let var_usage x : usage_relation =
    fun u1 u2 ->
      VariableSet.mem x u1
      && VariableSet.equal u2 (VariableSet.remove x u1)
  let same_usage info1 info2 : usage_relation =
    fun u1 u2 -> info1.usage u1 u2 && info2.usage u1 u2
  let disjoint_usage info1 info2 : usage_relation =
    fun u_in u_out ->
      VariableSet.exists_usage_subset u_in (fun u_mid ->
        info1.usage u_in u_mid
        && info2.usage u_mid u_out
        )
  let disjoint_usage_with info1 x info2 : usage_relation =
    fun u_in u_out ->
      VariableSet.exists_usage_subset u_in (fun u_mid ->
        not (VariableSet.mem x u_in)
        && info1.usage u_in u_mid
        && info2.usage (VariableSet.add x u_mid) u_out
        && not (VariableSet.mem x u_out)
      )
  let disjoint_usage_branch info0 x1 info1 x2 info2 : usage_relation =
    fun u_in u_out ->
      VariableSet.exists_usage_subset u_in (fun u_mid ->
        (* variables x1 and x2 should not appear in u_in or u_out at all *)
        (VariableSet.is_empty @@ VariableSet.inter
          (VariableSet.union u_in u_out)
          (VariableSet.of_list [x1;x2]))
        && info0.usage u_in u_mid
        && info1.usage (VariableSet.add x1 u_mid) u_out
        && info2.usage (VariableSet.add x2 u_mid) u_out
        )
end

module Typing = struct
  type type_information = (Type.t,Expr.t) TypeInformation.t
  open TypeInformation
  let string_of_info = TypeInformation.string_of_info Type.string_of_t Expr.pretty_string_of_t
  let pp_info info = print_string @@ string_of_info info
  let assert_type = TypeInformation.assert_type Type.string_of_t

  let assert_arrow_type (alpha : Type.t) : Type.t * Type.t =
    match alpha with
    | Type.Arrow (alpha1, alpha2) -> (alpha1, alpha2)
    | _ -> terr @@ "Expected an arrow type, received: " ^ Type.string_of_t alpha

  let assert_sum_type (alpha : Type.t) : Type.t * Type.t =
    match alpha with
    | Type.Sum (alpha1, alpha2) -> (alpha1, alpha2)
    | _ -> terr @@ "Expected a sum type, received: " ^ Type.string_of_t alpha

  let rec typecheck' (ctx : Type.t VariableMap.t) (a : Expr.t) : type_information = 
    match a with
    | Var x ->
      {
        expr = Var x;
        tp = type_of_var ctx x;
        usage = var_usage x
      }

    | Let (a1,x,a2) ->
      let info1 = typecheck' ctx a1 in
      let ctx' = VariableMap.add x info1.tp ctx in
      let info2 = typecheck' ctx' a2 in
      {
        expr = Let(Annot(info1.expr, info1.tp), x, info2.expr);
        tp = info2.tp;
        usage = disjoint_usage_with info1 x info2
      }

    | Zero tp ->
      {
        expr = Zero tp;
        tp = tp;
        usage = fun u1 u2 -> VariableSet.subset u2 u1
      }

    | Annot (a',alpha) ->
      let info' = typecheck' ctx a' in
      assert_type alpha info'.tp;
      {
        expr = Annot(info'.expr, alpha);
        tp = alpha;
        usage = info'.usage
      }

    | Plus (a1,a2) ->
      let info1 = typecheck' ctx a1 in
      let info2 = typecheck' ctx a2 in
      assert_type info1.tp info2.tp;
      {
        expr = Plus (info1.expr, info2.expr);
        tp = info1.tp;
        usage = same_usage info1 info2
      }

    | Const r ->
      {
        expr = Const r;
        tp = Unit;
        usage = VariableSet.equal
      }

    | Scale (a1,a2) ->
      let info1 = typecheck' ctx a1 in
      assert_type Unit info1.tp;
      let info2 = typecheck' ctx a2 in
      {
        expr = Scale(info1.expr, info2.expr);
        tp = info2.tp;
        usage = disjoint_usage info1 info2
      }

    | Pair (a1,a2) ->
      let info1 = typecheck' ctx a1 in
      let info2 = typecheck' ctx a2 in
      {
        expr = Pair (info1.expr, info2.expr);
        tp = Sum (info1.tp, info2.tp);
        usage = same_usage info1 info2
      }

    | Case(a',x1,a1,x2,a2) ->
      let info' = typecheck' ctx a' in
      let (tp1,tp2) = assert_sum_type info'.tp in
      let ctx1 = VariableMap.add x1 tp1 ctx in
      let ctx2 = VariableMap.add x2 tp2 ctx in

      let info1 = typecheck' ctx1 a1 in
      let info2 = typecheck' ctx2 a2 in
      assert_type info1.tp info2.tp;
      {
        expr = Case(Annot(info'.expr, info'.tp),x1,info1.expr, x2, info2.expr);
        tp = info1.tp;
        usage = disjoint_usage_branch info' x1 info1 x2 info2
      }

    | Lambda (x,alpha,a') ->
      let info' = typecheck' (VariableMap.add x alpha ctx) a' in
      {
        expr = Lambda (x, alpha, info'.expr);
        tp = Arrow(alpha,info'.tp);
        usage = fun u1 u2 ->
          not VariableSet.(mem x (union u1 u2))
          && info'.usage (VariableSet.add x u1) u2
      }

    |App (a1,a2) ->
      let info1 = typecheck' ctx a1 in
      let info2 = typecheck' ctx a2 in
      let (tp1,tp2) = assert_arrow_type info1.tp in
      assert_type tp1 info2.tp;
      {
        expr =App (Annot (info1.expr,info1.tp), info2.expr);
        tp = tp2;
        usage = disjoint_usage info1 info2
      }

  let typecheck a =
    let info = typecheck' VariableMap.empty a in
    (* linearity check implies info.usage(0,0) *)
    match info.usage VariableSet.empty VariableSet.empty with
    | true -> info
    | false ->
      terr @@ "Linearity check failed in the usage relation:\n" ^ string_of_info info

end
