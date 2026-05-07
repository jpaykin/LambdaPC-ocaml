open Scalars
open Ident

module Type = Ast.Type
module Expr = Ast.Expr

module HOAS = struct

  let var x : Expr.t = Expr.t_of_node (Var x)
  let zero ?ty:ty_opt () : Expr.t = Expr.t_of_node ~loc:None ~ty:ty_opt Expr.Zero
  let (+) a1 a2 = Expr.t_of_node (Expr.Plus (a1,a2))
  let const x = Expr.t_of_node (Expr.Const x)
  let ( * ) a1 a2 = Expr.t_of_node (Expr.Scale (a1,a2))
  let case a fx fz =
      let x = Ident.fresh() in
      let z = Ident.fresh() in
      Expr.t_of_node (Expr.Case{scrut=a; x1=x; a1=fx (var x); x2=z; a2=fz (var z)})
  
  let lambda tp (f : Expr.t -> Expr.t) =
      let x = Ident.fresh() in
      Expr.t_of_node (Expr.Lambda{x; tp; body=f (var x)})

  let (@) a1 a2 = Expr.t_of_node (Expr.App (a1, a2))
  let pair a1 a2 = Expr.t_of_node (Expr.Pair (a1,a2))

  let u = Type.t_of_node Unit
  let (++) alpha1 alpha2 = Type.t_of_node (Sum (alpha1, alpha2))
  let lolli alpha1 alpha2 = Type.t_of_node (Arrow (alpha1, alpha2))

end

module Val = struct

    type t =
      | Const of int
      | Pair of t * t
      | Lambda of Ident.t * Type.t * Expr.t

    let rec string_of_t v =
          match v with
          | Const c -> string_of_int c
          | Pair (v1, v2) -> "(" ^ string_of_t v1 ^ ", " ^ string_of_t v2 ^ ")"
          | Lambda (x, tp, a) ->
              "Lambda(" ^ Ident.string_of_t x ^ ":" ^ Type.string_of_t tp ^ ". " ^ Expr.string_of_t a ^ ")"


    let rec pretty_string_of_t v =
          match v with
          | Const c -> string_of_int c
          | Pair (Const 0, Const 0) -> "I"
          | Pair (Const 1, Const 0) -> "X"
          | Pair (Const 0, Const 1) -> "Z"
          | Pair (Const 1, Const 1) -> "Y"
          | Pair (v1, v2) -> "(" ^ pretty_string_of_t v1 ^ ", " ^ pretty_string_of_t v2 ^ ")"
          | Lambda (x, tp, a) ->
              "lambda x" ^ Ident.string_of_t x ^ ":" ^ Type.string_of_t tp ^ ". " ^ Expr.pretty_string_of_t a

    let rec expr_of_t v =
      match v with
      | Const c -> HOAS.const c
      | Pair (v1, v2) -> HOAS.pair (expr_of_t v1) (expr_of_t v2)
      | Lambda (x, tp, a) -> Expr.t_of_node (Expr.Lambda {x; tp; body=a})


    let rec map (f : int -> int) (v : t) : t =
        match v with
        | Const r -> Const (f r)
        | Pair (v1, v2) -> Pair (map f v1, map f v2)
        | Lambda (x,tp,a) -> Lambda (x,tp, Expr.map f a)

    let alpha_equiv v1 v2 = Expr.alpha_equiv (expr_of_t v1) (expr_of_t v2)

  end

module Eval (Zd : Z_SIG) = struct

  let rec vzero (ltp : Type.t) : Val.t =
    match ltp.node with
    | Unit -> Val.Const 0
    | Sum (ltp1, ltp2) ->
        let v1 = vzero ltp1 in
        let v2 = vzero ltp2 in
        Val.Pair (v1, v2)
    | Arrow (ltp1, ltp2) ->
        let x = Ident.fresh() in
        let v = vzero ltp2 in
        Val.Lambda (x, ltp1, Val.expr_of_t v)


  let rec vplus (v1 : Val.t) (v2 : Val.t) : Val.t =
    match v1, v2 with
    | Val.Const c1, Val.Const c2 -> Val.Const (Zd.normalize(c1 + c2))
    | Val.Pair (a1, b1), Val.Pair (a2, b2) ->
      Val.Pair (vplus a1 a2, vplus b1 b2)
    | Val.Lambda (x1, tp1, a1), Val.Lambda (x2, tp2, a2) ->
        if tp1 = tp2 then
            let fresh_x = Ident.fresh() in
            let a1' = Expr.rename_var x1 fresh_x a1 in
            let a2' = Expr.rename_var x2 fresh_x a2 in
            Val.Lambda (fresh_x, tp1, HOAS.(a1' + a2'))
        else
            failwith "vplus: Lambda types do not match"
    | _, _ -> failwith "vplus: mismatched values"

  let rec vscale (v1 : int) (v2 : Val.t) : Val.t =
    match v2 with
    | Val.Const c -> Val.Const (v1 * c)
    | Val.Pair (a, b) -> Val.Pair (vscale v1 a, vscale v1 b)
    | Val.Lambda (x, tp, a) -> Val.Lambda (x, tp, HOAS.(const v1 * a))

  let rec eval (ctx : Val.t VariableMap.t) (a : Expr.t) : Val.t =
    match a.node with
    | Var x -> (try VariableMap.find x ctx with Not_found -> failwith "Unbound variable")
    | Let{x;a;body} ->
      let v = eval ctx a in
      let ctx' = VariableMap.add x v ctx in
      eval ctx' body
    | Zero -> 
      (match a.ty with
      | Some tp -> vzero tp
      | None    -> failwith "No type annotation found for Zero"
      )
    | Const c -> Val.Const (Zd.normalize c)
    | Plus (a1, a2) -> vplus (eval ctx a1) (eval ctx a2)
    | Scale (a1, a2) ->
        (match eval ctx a1 with
        | Val.Const c -> vscale c (eval ctx a2)
        | _ -> failwith "Scale: first argument must be a scalar")
    | Pair (a1, a2) -> Val.Pair (eval ctx a1, eval ctx a2)
    | Case {scrut; x1; a1; x2; a2} ->
        (match eval ctx scrut with
        | Val.Pair (v1, v2) ->
            let ctx1 = VariableMap.add x1 v1 ctx in
            let ctx2 = VariableMap.add x2 v2 ctx in
            vplus (eval ctx1 a1) (eval ctx2 a2)
        | _ -> failwith "Case: scrutinee must be a pair")
    | Lambda {x; tp; body} -> Val.Lambda (x, tp, body)
    | App (a1, a2) ->
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
  type usage_relation = IdentSet.t -> IdentSet.t -> bool

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

  let type_of_var (gamma : 'a VariableMap.t) (x : Ident.t) : 'a =
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
      IdentSet.mem x u1
      && IdentSet.equal u2 (IdentSet.remove x u1)
  let same_usage info1 info2 : usage_relation =
    fun u1 u2 -> info1.usage u1 u2 && info2.usage u1 u2
  let disjoint_usage info1 info2 : usage_relation =
    fun u_in u_out ->
      IdentSet.exists_usage_subset u_in (fun u_mid ->
        info1.usage u_in u_mid
        && info2.usage u_mid u_out
        )
  let disjoint_usage_with info1 x info2 : usage_relation =
    fun u_in u_out ->
      IdentSet.exists_usage_subset u_in (fun u_mid ->
        not (IdentSet.mem x u_in)
        && info1.usage u_in u_mid
        && info2.usage (IdentSet.add x u_mid) u_out
        && not (IdentSet.mem x u_out)
      )
  let disjoint_usage_branch info0 x1 info1 x2 info2 : usage_relation =
    fun u_in u_out ->
      IdentSet.exists_usage_subset u_in (fun u_mid ->
        (* variables x1 and x2 should not appear in u_in or u_out at all *)
        (IdentSet.is_empty @@ IdentSet.inter
          (IdentSet.union u_in u_out)
          (IdentSet.of_list [x1;x2]))
        && info0.usage u_in u_mid
        && info1.usage (IdentSet.add x1 u_mid) u_out
        && info2.usage (IdentSet.add x2 u_mid) u_out
        )
end

module Typing = struct
  type type_information = (Type.t,Expr.t) TypeInformation.t
  open TypeInformation
  let string_of_info = TypeInformation.string_of_info Type.string_of_t Expr.pretty_string_of_t
  let pp_info info = print_string @@ string_of_info info
  let assert_type = TypeInformation.assert_type Type.string_of_t

  let assert_arrow_type (alpha : Type.t) : Type.t * Type.t =
    match alpha.node with
    | Type.Arrow (alpha1, alpha2) -> (alpha1, alpha2)
    | _ -> terr @@ "Expected an arrow type, received: " ^ Type.string_of_t alpha

  let assert_sum_type (alpha : Type.t) : Type.t * Type.t =
    match alpha.node with
    | Type.Sum (alpha1, alpha2) -> (alpha1, alpha2)
    | _ -> terr @@ "Expected a sum type, received: " ^ Type.string_of_t alpha

  let annot (a : Expr.t) (tp : Type.t) : Expr.t =
    match a.ty with
    | Some tp0 -> assert_type tp tp0; a
    | None -> { a with ty=Some tp }

  let rec typecheck' (ctx : Type.t VariableMap.t) (a : Expr.t) : type_information = 
    match a.node with
    | Var x ->
      {
        expr = HOAS.var x;
        tp = type_of_var ctx x;
        usage = var_usage x
      }

    | Let {x;a;body} ->
      let info1 = typecheck' ctx a in
      let ctx' = VariableMap.add x info1.tp ctx in
      let info2 = typecheck' ctx' body in
      {
        expr = Expr.t_of_node (Let{x; a=annot (info1.expr) (info1.tp); body=info2.expr});
        tp = info2.tp;
        usage = disjoint_usage_with info1 x info2
      }

    | Zero ->
      let tp = (match a.ty with Some tp -> tp | None -> terr "No type associated with Zero") in
      {
        expr = HOAS.zero ~ty:tp ();
        tp;
        usage = fun u1 u2 -> IdentSet.subset u2 u1
      }

    | Plus (a1,a2) ->
      let info1 = typecheck' ctx a1 in
      let info2 = typecheck' ctx a2 in
      assert_type info1.tp info2.tp;
      {
        expr = HOAS.(info1.expr + info2.expr);
        tp = info1.tp;
        usage = same_usage info1 info2
      }

    | Const r ->
      {
        expr = HOAS.const r;
        tp = Type.t_of_node Unit;
        usage = IdentSet.equal
      }

    | Scale (a1,a2) ->
      let info1 = typecheck' ctx a1 in
      assert_type (Type.t_of_node Unit) info1.tp;
      let info2 = typecheck' ctx a2 in
      {
        expr = HOAS.(info1.expr * info2.expr);
        tp = info2.tp;
        usage = disjoint_usage info1 info2
      }

    | Pair (a1,a2) ->
      let info1 = typecheck' ctx a1 in
      let info2 = typecheck' ctx a2 in
      {
        expr = HOAS.pair (info1.expr) (info2.expr);
        tp = HOAS.(info1.tp ++ info2.tp);
        usage = same_usage info1 info2
      }

    | Case{scrut;x1;a1;x2;a2} ->
      let info' = typecheck' ctx scrut in
      let (tp1,tp2) = assert_sum_type info'.tp in
      let ctx1 = VariableMap.add x1 tp1 ctx in
      let ctx2 = VariableMap.add x2 tp2 ctx in

      let info1 = typecheck' ctx1 a1 in
      let info2 = typecheck' ctx2 a2 in
      assert_type info1.tp info2.tp;
      {
        expr = Expr.t_of_node (Case{scrut = annot info'.expr info'.tp;x1;a1=info1.expr; x2; a2=info2.expr});
        tp = info1.tp;
        usage = disjoint_usage_branch info' x1 info1 x2 info2
      }

    | Lambda {x;tp=alpha;body} ->
      let info' = typecheck' (VariableMap.add x alpha ctx) body in
      {
        expr = Expr.t_of_node (Lambda {x; tp=alpha; body=info'.expr});
        tp = HOAS.lolli alpha info'.tp;
        usage = fun u1 u2 ->
          not IdentSet.(mem x (union u1 u2))
          && info'.usage (IdentSet.add x u1) u2
      }

    | App (a1,a2) ->
      let info1 = typecheck' ctx a1 in
      let info2 = typecheck' ctx a2 in
      let (tp1,tp2) = assert_arrow_type info1.tp in
      assert_type tp1 info2.tp;
      {
        expr = HOAS.(annot info1.expr info1.tp @ info2.expr);
        tp = tp2;
        usage = disjoint_usage info1 info2
      }

  let typecheck a =
    let info = typecheck' VariableMap.empty a in
    (* linearity check implies info.usage(0,0) *)
    match info.usage IdentSet.empty IdentSet.empty with
    | true -> info
    | false ->
      terr @@ "Linearity check failed in the usage relation:\n" ^ string_of_info info

end
