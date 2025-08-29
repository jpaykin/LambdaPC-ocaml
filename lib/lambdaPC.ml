

type pctype =
    Pauli | PTensor of pctype * pctype

let rec ltype_of_pctype (tp : pctype) : LambdaC.ltype =
    match tp with
    | Pauli -> Sum (Unit, Unit)
    | PTensor (tp1, tp2) -> Sum (ltype_of_pctype tp1, ltype_of_pctype tp2)

module Variable = LambdaC.Variable
module VariableMap = LambdaC.VariableMap

module Expr (Zd : Qudits.Z_SIG) = struct
  module E = LambdaC.Expr(Zd)

  type t =
    | Var of Variable.t
    | LExpr of LambdaC.Expr(Zd).t
    | Phase of LambdaC.Expr(Zd).t * t
    | Prod of t * t
    | Pow of t * int
    | CasePauli of t * t * t
    | In1 of t
    | In2 of t
    | CasePTensor of t * Variable.t * t * Variable.t * t
  
  (*val string_of_t : t -> string*)
  let rec string_of_t e = 
    match e with
      | Var x -> "Var(" ^ string_of_int x ^ ")"
      | LExpr le -> "LExpr(" ^ E.string_of_t le ^ ")"
      | Phase (a, t) -> "Phase(" ^ E.string_of_t a ^ ", " ^ string_of_t t ^ ")"
      | Prod (t1, t2) -> "Prod(" ^ string_of_t t1 ^ ", " ^ string_of_t t2 ^ ")"
      | Pow (t, n) -> "Pow(" ^ string_of_t t ^ ", " ^ string_of_int n ^ ")"
      | CasePauli (e, t1, t2) -> "CasePauli(" ^ string_of_t e ^ ", " ^ string_of_t t1 ^ ", " ^ string_of_t t2 ^ ")"
      | In1 t -> "In1(" ^ string_of_t t ^ ")"
      | In2 t -> "In2(" ^ string_of_t t ^ ")"
      | CasePTensor (e, x1, t1, x2, t2) ->
        "CasePTensor(" ^ string_of_t e ^ ", " ^ string_of_int x1 ^ ", " ^ string_of_t t1 ^ ", " ^ string_of_int x2 ^ ", " ^ string_of_t t2 ^ ")" 

  let rec rename_var (from : Variable.t) (to_ : Variable.t) e =
      match e with
      | Var x ->
          if x = from then Var to_ else Var x
      | LExpr le ->
          LExpr (E.rename_var from to_ le)
      | Phase (a, t) ->
          Phase (a, rename_var from to_ t)
      | Prod (t1, t2) ->
          Prod (rename_var from to_ t1, rename_var from to_ t2)
      | Pow (t, n) ->
          Pow (rename_var from to_ t, n)
      | CasePauli (e, t1, t2) ->
          CasePauli (rename_var from to_ e, rename_var from to_ t1, rename_var from to_ t2)
      | In1 t ->
          In1 (rename_var from to_ t)
      | In2 t ->
          In2 (rename_var from to_ t)
      | CasePTensor (e, x1, e1, x2, e2) ->
          let e1' = if x1 = from then e1 else rename_var from to_ e1 in
          let e2' = if x1 = from then e2 else rename_var from to_ e2 in
          CasePTensor (
            rename_var from to_ e,
              x1, e1',
              x2, e2'
          )

end

module Val (Zd : Qudits.Z_SIG) = struct
  type t = {phase : Zd.t; value : LambdaC.Val(Zd).t }
  module V = LambdaC.Val(Zd)

  let string_of_t (cfg : t) : string =
    "<" ^ Zd.string_of_t cfg.phase ^ "> " ^ V.string_of_t cfg.value
end


module Eval (C : Qudits.CONVERSIONS) = struct
  module LCC = LambdaC.Conversions(C)
  open C
  open LCC

  module LEval = LambdaC.Eval(Zd)
  module V = LambdaC.Val(Zd)

  module LEval' = LambdaC.Eval(Zd')
  module V' = LambdaC.Val(Zd')

  module E = Expr(C.Zd)
  module Cfg = Val(C.Zd)

  let cprod_phase env (v1 : V.t) (v2 : V.t) : Zd.t =
    (*(d/2) (sgn (omega'(_v1_,_v2_) + sgn(_v1_ + _v2_)))*)

    let v1' : V'.t = val'_of_val v1 in
    let v2' : V'.t = val'_of_val v2 in 

    let v1'_plus_v2' : V'.t = LEval'.vplus env v1' v2' in

    let r : Zd0.t = C.sgn (LEval'.symplectic_form v1' v2') in
    let r' : Zd0.t = sgn v1'_plus_v2' in

    times_d2 Zd0.(r + r')

  let pow_phase env (v : V.t) (r : Zd.t) : Zd.t =
    let v' : V'.t = val'_of_val v in
    let v'' : V'.t = LEval'.vscale env (inc_d_d' r) v' in
    times_d2 (sgn v'')


    (*
  module M = LambdaC.Expr(Zd)
  type pconfig = { phase : int; value : M.lval }

  module C = Qudits.Conversions(Zd)

  let cprod_phase (v1 : lval) (v2 : lval) : Zd

  
  let rec eval (env : M.environment)
               (ctx : M.lval VariableMap.t)
               (e : pexpr) 
               : config =
    match e with
    | Var x ->
      let v = (try VariableMap.find x ctx with
        Not_found -> failwith "Unbound variable") in
      {phase = 0; value = v}
    | LExpr e' ->
      {phase = 0; value = M.eval env ctx e'}

    | Phase (a, t) ->
      match M.eval env ctx a, eval env ctx t with
      | M.VConst p, {phase=p0; value=v0} ->
        {phase=p+p0; value=v0}
      | _, _ -> failwith "Phase: first argument must be a scalar"

    | Prod (e1, e2) ->

    *)
    

end