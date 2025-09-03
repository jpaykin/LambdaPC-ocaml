open Scalars

type ltype =
    | Unit
    | Sum of ltype * ltype
    | Arrow of ltype * ltype


let rec string_of_ltype tp =
        match tp with
        | Unit -> "Unit"
        | Sum (t1, t2) -> "Sum(" ^ string_of_ltype t1 ^ ", " ^ string_of_ltype t2 ^ ")"
        | Arrow (t1, t2) -> "Arrow(" ^ string_of_ltype t1 ^ " -> " ^ string_of_ltype t2 ^ ")"


module Variable = struct
  type t = int
  let compare = Int.compare
end
module VariableMap = Map.Make(Variable)

type environment = { fresh_var : Variable.t ref }
let fresh (env : environment) : Variable.t =
  let x = !(env.fresh_var) in
  env.fresh_var := x+1;
  x

module Expr = struct

  type t =
      Var of Variable.t
    | Zero of ltype
    | Plus of t * t
    | Const of int
    | Scale of t * t
    | Pair of t * t
    | Case of t * Variable.t * t * Variable.t * t
    | Lambda of Variable.t * ltype * t
    | Apply of t * t

    let rec string_of_t e =
      match e with
      | Var x -> "Var(" ^ string_of_int x ^ ")"
      | Zero tp -> "Zero(" ^ string_of_ltype tp ^ ")"
      | Plus (e1, e2) -> "Plus(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Const c -> "Const(" ^ string_of_int c ^ ")"
      | Scale (e1, e2) -> "Scale(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Pair (e1, e2) -> "Pair(" ^ string_of_t e1 ^ ", " ^ string_of_t e2 ^ ")"
      | Case (scrut, x1, e1, x2, e2) ->
          "Case(" ^ string_of_t scrut ^ ", " ^ string_of_int x1 ^ ", " ^ string_of_t e1 ^ ", " ^ string_of_int x2 ^ ", " ^ string_of_t e2 ^ ")"
      | Lambda (x, tp, body) ->
          "Lambda(" ^ string_of_int x ^ ":" ^ string_of_ltype tp ^ ". " ^ string_of_t body ^ ")"
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


  end

module HOAS = struct
  let env : environment = { fresh_var = ref 0; }

  let var x = Expr.Var x
  let zero tp = Expr.Zero tp
  let (+) e1 e2 = Expr.Plus (e1,e2)
  let const x = Expr.Const x
  let ( * ) e1 e2 = Expr.Scale (e1,e2)
  let case e fx fz =
      let x = fresh env in
      let z = fresh env in
      Expr.Case(e, x, fx x, z, fz z)
  
  let lambda tp (f : Variable.t -> Expr.t) =
      let x = fresh env in
      Expr.Lambda (x, tp, f x)
  let (@) e1 e2 = Expr.Apply (e1, e2)
    
end

module Val = struct

    type t =
      | Const of int
      | Pair of t * t
      | Lambda of Variable.t * ltype * Expr.t

    let rec string_of_t v =
          match v with
          | Const c -> string_of_int c
          | Pair (v1, v2) -> "(" ^ string_of_t v1 ^ ", " ^ string_of_t v2 ^ ")"
          | Lambda (x, tp, e) ->
              "λ" ^ string_of_int x ^ ":" ^ string_of_ltype tp ^ ".(" ^ Expr.string_of_t e ^ ")"

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

  end

module Eval (Zd : Z_SIG) = struct
  let rec vzero (ltp : ltype) (env : environment) =
    match ltp with
    | Unit -> Val.Const 0
    | Sum (ltp1, ltp2) ->
        let v1 = vzero ltp1 env in
        let v2 = vzero ltp2 env in
        Val.Pair (v1, v2)
    | Arrow (ltp1, ltp2) ->
        let x = !(env.fresh_var) in
        env.fresh_var := x+1;
        let v = vzero ltp2 env in
        Val.Lambda (x, ltp1, Val.expr_of_t v)


  let rec vplus (env : environment) (v1 : Val.t) (v2 : Val.t) : Val.t =
    match v1, v2 with
    | Val.Const c1, Val.Const c2 -> Val.Const (Zd.normalize(c1 + c2))
    | Val.Pair (a1, b1), Val.Pair (a2, b2) ->
      Val.Pair (vplus env a1 a2, vplus env b1 b2)
    | Val.Lambda (x1, tp1, e1), Val.Lambda (x2, tp2, e2) ->
        if tp1 = tp2 then
            let fresh_x = fresh env in
            let e1' = Expr.rename_var x1 fresh_x e1 in
            let e2' = Expr.rename_var x2 fresh_x e2 in
            Val.Lambda (fresh_x, tp1, Expr.Plus (e1', e2'))
        else
            failwith "vplus: Lambda types do not match"
    | _, _ -> failwith "vplus: mismatched lval types"

  let rec vscale (env : environment) (v1 : int) (v2 : Val.t) : Val.t =
    match v2 with
    | Val.Const c -> Val.Const (v1 * c)
    | Val.Pair (a, b) -> Val.Pair (vscale env v1 a, vscale env v1 b)
    | Val.Lambda (x, tp, e) -> Val.Lambda (x, tp, Scale (Const v1, e))

  let rec eval (env : environment) (ctx : Val.t VariableMap.t) (e : Expr.t) : Val.t =
    match e with
    | Var x -> (try VariableMap.find x ctx with Not_found -> failwith "Unbound variable")
    | Zero tp -> vzero tp env
    | Const c -> Val.Const (Zd.normalize c)
    | Plus (e1, e2) -> vplus env (eval env ctx e1) (eval env ctx e2)
    | Scale (e1, e2) ->
        (match eval env ctx e1 with
        | Val.Const c -> vscale env c (eval env ctx e2)
        | _ -> failwith "Scale: first argument must be a scalar")
    | Pair (e1, e2) -> Val.Pair (eval env ctx e1, eval env ctx e2)
    | Case (scrut, x1, e1, x2, e2) ->
        (match eval env ctx scrut with
        | Val.Pair (v1, v2) ->
            let ctx1 = VariableMap.add x1 v1 ctx in
            let ctx2 = VariableMap.add x2 v2 ctx in
            vplus env (eval env ctx1 e1) (eval env ctx2 e2)
        | _ -> failwith "Case: scrutinee must be a pair")
    | Lambda (x, tp, body) -> Val.Lambda (x, tp, body)
    | Apply (e1, e2) ->
        (match eval env ctx e1 with
        | Val.Lambda (x, _tp, body) ->
            let arg = eval env ctx e2 in
            let ctx' = VariableMap.add x arg ctx in
            eval env ctx' body
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


(** Tests *)
(*
let lexpr_example_1 = Plus (Var 0, Const (Zd.t_of_int 3))
let lexpr_example_2 = Lambda (1, Unit, Plus (Var 1, Zero Unit))
let lexpr_example_3 = Apply (lexpr_example_2, lexpr_example_1)

let ctx_example = VariableMap.singleton 0 (VConst (Zd.t_of_int 39))
let env_example = { fresh_var = ref 100 }
let lval_example = eval env_example ctx_example lexpr_example_3
*)
end



module Conversions (S : SCALARS) = struct
  
  open S
  module E = Eval(Zd')

  let sgn (v' : Val.t) : Zd0.t =
    let f (x : int) = Zd'.int_of_t (inc_d_d' (mod_d'_d (Zd'.t_of_int(x)))) in
    let v'' = Val.map f v' in
    let r' = E.symplectic_form v' v'' in
    div_d r'

end