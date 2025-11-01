open Scalars

module Type = struct

  type t =
    Pauli | PTensor of t * t

  let rec ltype_of_t (tp : t) : LambdaC.Type.t =
    match tp with
    | Pauli -> Sum (Unit, Unit)
    | PTensor (tp1, tp2) -> Sum (ltype_of_t tp1, ltype_of_t tp2)

  let rec string_of_t (tp : t) : string =
    match tp with
    | Pauli -> "Pauli" 
    | PTensor (Pauli, Pauli) -> "Pauli ** Pauli"
    | PTensor (Pauli, tp2) -> "Pauli ** (" ^ string_of_t tp2 ^ ")"
    | PTensor (tp1, Pauli) -> "(" ^ string_of_t tp1 ^ ") ** Pauli"
    | PTensor (tp1, tp2) -> "(" ^ string_of_t tp1 ^ ") ** (" ^ string_of_t tp2 ^ ")"
end

module Variable = LambdaC.Variable
module VariableMap = LambdaC.VariableMap


module Expr = struct

  type t =
    | Var   of Variable.t
    | Let   of t * Variable.t * t
    | LExpr of LambdaC.Expr.t
    | Phase of LambdaC.Expr.t * t
    | Prod  of t * t
    | Pow   of t * int
    | CasePauli of t * t * t
    | In1   of t * Type.t
    | In2   of Type.t * t
    | CasePTensor of t * Variable.t * t * Variable.t * t
    | Apply of pc * t
    | Force of p

  and pc = Lam of (Variable.t * Type.t * t)

  and p = Suspend of t
  
  (*val string_of_t : t -> string*)
  let rec string_of_t e = 
    match e with
      | Var x -> "Var(" ^ string_of_int x ^ ")"
      | Let (e1, x, e2) -> "Let(" ^ string_of_t e1 ^ ", " ^ string_of_int x ^ ", " ^ string_of_t e2 ^ ")"
      | LExpr le -> "LExpr(" ^ LambdaC.Expr.string_of_t le ^ ")"
      | Phase (a, t) -> "Phase(" ^ LambdaC.Expr.string_of_t a ^ ", " ^ string_of_t t ^ ")"
      | Prod (t1, t2) -> "Prod(" ^ string_of_t t1 ^ ", " ^ string_of_t t2 ^ ")"
      | Pow (t, n) -> "Pow(" ^ string_of_t t ^ ", " ^ string_of_int n ^ ")"
      | CasePauli (e, t1, t2) -> "CasePauli(" ^ string_of_t e ^ ", " ^ string_of_t t1 ^ ", " ^ string_of_t t2 ^ ")"
      | In1 (t,_tp) -> "In1(" ^ string_of_t t ^ ")"
      | In2 (_,t) -> "In2(" ^ string_of_t t ^ ")"
      | CasePTensor (e, x1, t1, x2, t2) ->
        "CasePTensor(" ^ string_of_t e ^ ", " ^ string_of_int x1 ^ ", " ^ string_of_t t1 ^ ", " ^ string_of_int x2 ^ ", " ^ string_of_t t2 ^ ")"

      | Apply (f, e) -> string_of_pc f ^ " @ " ^ string_of_t e
      | Force e' -> string_of_p e'
  and string_of_pc f = match f with
      | Lam (x, tp, e) -> "lambda " ^ string_of_int x ^ " : " ^ Type.string_of_t tp ^ ". " ^ string_of_t e
  and string_of_p e = match e with
      | Suspend e' -> string_of_t e'

let rec pretty_string_of_t e = 
    match e with
      | Var x -> "x" ^ string_of_int x
      | Let (e1, x, e2) -> "let " ^ string_of_int x ^ " = " ^ pretty_string_of_t e1 ^ " in " ^ pretty_string_of_t e2
      | LExpr le -> LambdaC.Expr.pretty_string_of_t le
      | Phase (a, t) -> "<" ^ LambdaC.Expr.pretty_string_of_t a ^ "> " ^ pretty_string_of_t t
      | Prod (t1, t2) -> "(" ^ pretty_string_of_t t1 ^ ") * (" ^ pretty_string_of_t t2 ^ ")"
      | Pow (t, n) -> "(" ^ pretty_string_of_t t ^ ")^(" ^ string_of_int n ^ ")"
      | CasePauli (e, t1, t2) -> "case " ^ pretty_string_of_t e ^ " of { X -> " ^ pretty_string_of_t t1 ^ " | Z -> " ^ pretty_string_of_t t2 ^ "}"
      | In1 (t,_tp) -> "in1(" ^ pretty_string_of_t t ^ ")"
      | In2 (_,t) -> "in2(" ^ pretty_string_of_t t ^ ")"
      | CasePTensor (e, x1, t1, x2, t2) ->
        "case " ^ pretty_string_of_t e
                ^ " of { in1 x" ^  string_of_int x1 ^ " -> " ^ pretty_string_of_t t1 
                ^ " | in2 x" ^ string_of_int x2 ^ " -> " ^ pretty_string_of_t t2 ^ "}"

      | Apply (f, e) -> "(" ^ pretty_string_of_pc f ^ ") @ (" ^ pretty_string_of_t e ^ ")"
      | Force e' -> pretty_string_of_p e'
  and pretty_string_of_pc f = match f with
      | Lam (x, tp, e) -> "lambda x" ^ string_of_int x ^ " : " ^ Type.string_of_t tp ^ ". " ^ pretty_string_of_t e
  and pretty_string_of_p e = match e with
      | Suspend e' -> pretty_string_of_t e'

  let rec rename_var (from : Variable.t) (to_ : Variable.t) e =
      match e with
      | Var x ->
          if x = from then Var to_ else Var x
      | Let (e1, x, e2) ->
          let e1' = rename_var from to_ e1 in
          let e2' = if x = from then e2 else rename_var from to_ e2 in
          Let (e1', x, e2')
      | LExpr le ->
          LExpr (LambdaC.Expr.rename_var from to_ le)
      | Phase (a, t) ->
          Phase (a, rename_var from to_ t)
      | Prod (t1, t2) ->
          Prod (rename_var from to_ t1, rename_var from to_ t2)
      | Pow (t, n) ->
          Pow (rename_var from to_ t, n)
      | CasePauli (e, t1, t2) ->
          CasePauli (rename_var from to_ e, rename_var from to_ t1, rename_var from to_ t2)
      | In1 (t, tp2) ->
          In1 (rename_var from to_ t, tp2)
      | In2 (tp1, t) ->
          In2 (tp1, rename_var from to_ t)
      | CasePTensor (e, x1, e1, x2, e2) ->
          let e1' = if x1 = from then e1 else rename_var from to_ e1 in
          let e2' = if x1 = from then e2 else rename_var from to_ e2 in
          CasePTensor (
            rename_var from to_ e,
              x1, e1',
              x2, e2'
          )
      | Apply (f, e') -> (* no free variables in f *) Apply (f, rename_var from to_ e')
      | Force e' -> (* no free variables in e' *) Force e'

end

module Val = struct
  type t = { phase : int; value : LambdaC.Val.t }

  let string_of_t (cfg : t) : string =
    "<" ^ string_of_int cfg.phase ^ "> " ^ LambdaC.Val.pretty_string_of_t cfg.value

  let pure (v : LambdaC.Val.t) = { phase = 0; value = v }

end



module HOAS = struct
  let fresh = LambdaC.HOAS.fresh

  let var (x : Variable.t) = Expr.Var x
  let letin e f =
    let x = fresh() in
    Expr.Let (e, x, f x)
  let vec a = Expr.LExpr a
  let phase a e = Expr.Phase(a, e)
  let ( * ) e1 e2 = Expr.Prod(e1,e2)
  let pow e n = Expr.Pow(e,n)
  let caseofP e ex ez = Expr.CasePauli(e,ex,ez)
  let in1 e tp2 = Expr.In1 (e,tp2)
  let in2 tp1 e = Expr.In2 (tp1, e)
  let caseof e b1 b2 =
    let x1 = fresh() in
    let x2 = fresh() in
    Expr.CasePTensor(e, x1, b1 x1, x2, b2 x2)

  let lambda tp (f : Variable.t -> Expr.t) =
      let x = fresh() in
      Expr.Lam (x, tp, f x)
  let (@) e1 e2 = Expr.Apply (e1, e2)

  let suspend e = Expr.Suspend e
  let force e = Expr.Force e
end

(**************)
(* EVALUATION *)
(**************)

module PhaseEnvironment (Zd : Z_SIG) = struct
  type t = Zd.t ref
  let init : t = {contents = Zd.t_of_int(0)}

  let add_phase (ref : t) (r : Zd.t) =
    ref := Zd.(!ref + r)
  let add_integer_phase (ref : t) (r : int) =
    add_phase ref (Zd.t_of_int r)
end
module Eval (S : SCALARS) = struct
  open S
  open Val

  (* variable environment *)
  module VarEnv = LambdaC.VariableEnvironment
  let var_env : VarEnv.t ref = {contents = VarEnv.init}
  let set_variable_environment (env : VarEnv.t) = var_env := env
  let fresh () : Variable.t = VarEnv.fresh !var_env


  (* phase environment *)

  module LEval  = LambdaC.Eval(Zd)
  module LEval' = LambdaC.Eval(Zd')

  let sgn (v' : LambdaC.Val.t) : Zd0.t =
    let f (x : int) = Zd'.int_of_t (inc_d_d' (mod_d'_d (Zd'.t_of_int(x)))) in
    let v'' = LambdaC.Val.map f v' in
    let r' = LEval'.symplectic_form v' v'' in
    div_d r'

  
  let add_phase (r : int) (cfg : Val.t) =
    {
      phase = r + cfg.phase;
      value = cfg.value
    }

  let cprod_phase (v1 : LambdaC.Val.t) (v2 : LambdaC.Val.t) : Zd.t =
    (*(d/2) (sgn (omega'(_v1_,_v2_) + sgn(_v1_ + _v2_)))*)

    let v1'_plus_v2' = LEval'.vplus v1 v2 in

    let r  : S.Zd0.t = S.sgn (LEval'.symplectic_form v1 v2) in
    let r' : S.Zd0.t = sgn v1'_plus_v2' in

    times_d2 Zd0.(r + r')

  let cfg_prod (cfg1 : Val.t) (cfg2 : Val.t) : Val.t =
      let {Val.phase = r1; value = v1} = cfg1 in
      let {Val.phase = r2; value = v2} = cfg2 in
      print_endline ("v1: " ^ Val.string_of_t cfg1);
      print_endline ("v2: " ^ Val.string_of_t cfg2);
      {
        phase = Zd.normalize(r1 + r2 + Zd.int_of_t(cprod_phase v1 v2));
        value = LEval.vplus v1 v2
      }

  let pow_phase (v : LambdaC.Val.t) (r : int) : Zd.t =
    let v' = LEval'.vscale r v in
    times_d2 (sgn v')


  let cfg_pow (cfg : Val.t) (r : int) =
      let {Val.phase = r'; value = v'} = cfg in
      {
        phase = Zd.normalize(r * r' + Zd.int_of_t(pow_phase v' r));
        value = LEval.vscale r v'
      }

  let case_phase (rx : int) (rz : int) : int =
    (* d/2 sgn(_rx_ _rz_) *)
    let r : Zd.t = S.times_d2 (S.sgn Zd'.(Zd'.t_of_int(rx) * Zd'.t_of_int(rz))) in
    Zd.int_of_t(r)

  let rec eval (ctx : LambdaC.Val.t VariableMap.t)
               (e : Expr.t) 
               : Val.t =
    match e with
    | Var x -> 
      Val.pure (try VariableMap.find x ctx with Not_found -> failwith "Unbound variable")
    | Let (e1, x, e2) ->
      let {phase = r1; value = v1} = eval ctx e1 in
      let ctx' = VariableMap.add x v1 ctx in
      add_phase r1 (eval ctx' e2)
    | LExpr e' -> Val.pure (LEval.eval ctx e')
    | Phase (a,e') ->
      (match LEval.eval ctx a with
      | Const r -> add_phase r (eval ctx e')
      | _ -> failwith "Expected phase to evaluate to a constant"
      )
    | Prod (e1,e2) ->
      cfg_prod (eval ctx e1) (eval ctx e2)

    | Pow (e', r) ->
      cfg_pow (eval ctx e') r

    | CasePauli(e,ex,ez) -> 
      (match eval ctx e with
      | {phase = r; value = LambdaC.Val.Pair (LambdaC.Val.Const rx, LambdaC.Val.Const rz)} ->
        let cfgx = eval ctx (Expr.Pow(ex, rx)) in
        let cfgz = eval ctx (Expr.Pow(ez, rz)) in
        let k    = case_phase rx rz in
        add_phase (r + k) (cfg_prod cfgz cfgx)

      | _ -> failwith "eval [CasePauli] -> expected a value of Pauli type")

    | In1 (e',tp2) ->
      let {phase = r; value = v} = eval ctx e' in
      let v' = LambdaC.Val.Pair (v, LEval.vzero (Type.ltype_of_t tp2)) in
      {phase = r; value = v'}
    | In2 (tp1, e') ->
      let {phase = r; value = v} = eval ctx e' in
      let v' = LambdaC.Val.Pair (LEval.vzero (Type.ltype_of_t tp1), v) in
      {phase = r; value = v'}

    | CasePTensor(e', x1, e1, x2, e2) ->
      (match eval ctx e' with
      | {phase = r; value = LambdaC.Val.Pair (v1,v2)} ->
        print_endline ("got pair: [" ^ LambdaC.Val.pretty_string_of_t v1
                                ^ ", " ^ LambdaC.Val.pretty_string_of_t v2
                                ^ "]");
        let ctx1 = VariableMap.add x1 v1 ctx in
        let cfg1 = eval ctx1 e1 in
        print_endline "evaluated e1";
        let ctx2 = VariableMap.add x2 v2 ctx in
        let cfg2 = eval ctx2 e2 in
        print_endline "evaluated e2";
        let cfg = cfg_prod cfg1 cfg2 in
        print_endline "got result";
        add_phase r cfg

      | _ -> failwith "eval [CasePTensor] -> expected a pair")

    | Apply (Lam (x,_,e), e') ->
        let {phase = r; value = v} = eval ctx e' in
        let ctx' = VariableMap.singleton x v in
        add_phase r (eval ctx' e)
    | Force (Suspend e') -> eval (VariableMap.empty) e'

  let evalClosed e = eval VariableMap.empty e
end