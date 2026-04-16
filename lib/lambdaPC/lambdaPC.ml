open Scalars
open Ident

module Type = struct

  type t = { loc : Loc.t option; node : node }
  and node =
      | Pauli
      | PTensor of t * t

  let t_of_node n = {loc = None; node = n}

  let pauli : t = t_of_node Pauli
  let ( ** ) tp1 tp2 : t = t_of_node @@ PTensor(tp1,tp2)


  let rec ltype_of_t (tp : t) : LambdaC.Type.t =
    match tp.node with
    | Pauli -> LambdaC.HOAS.(u ++ u)
    | PTensor (tp1, tp2) -> LambdaC.HOAS.(lolli (ltype_of_t tp1) (ltype_of_t tp2))

  let rec t_of_ltype (tp : LambdaC.Type.t) : t option =
    match tp.node with
    | Sum(tp1,tp2) -> 
      (match tp1.node, tp2.node with
      | Unit, Unit -> Some (t_of_node Pauli)
      | _, _ ->
        (match t_of_ltype tp1, t_of_ltype tp2 with
        | Some tp1', Some tp2' ->
          Some (t_of_node (PTensor (tp1',tp2')))
        | _, _ -> None
        )
      )
    | _ -> None

  let rec string_of_t (tp : t) : string =
    match tp.node with
    | Pauli -> "Pauli" 
    | PTensor (tp1, tp2) -> 
        (match tp1.node, tp2.node with
        | Pauli, Pauli -> "Pauli ** Pauli"
        | Pauli, _ -> "Pauli ** (" ^ string_of_t tp2 ^ ")"
        | _, Pauli -> "(" ^ string_of_t tp1 ^ ") ** Pauli"
        | _, _ -> "(" ^ string_of_t tp1 ^ ") ** (" ^ string_of_t tp2 ^ ")")
end

module Expr = struct

  type t = { loc : Loc.t option; ty : Type.t option; node : node }
  and node =
    | Var   of Ident.t
    | Let   of { x : Ident.t; expr : t; body : t }
    | LExpr of LambdaC.Expr.t
    | Phase of LambdaC.Expr.t * t
    | Prod  of t * t
    | Pow   of t * LambdaC.Expr.t
    | CasePauli   of { scrut : t; tx : t; tz : t }
    | In1 of { tp : Type.t; v : t }
    | In2 of { tp : Type.t; v : t }
    | CasePTensor of
        { scrut : t
        ; x1    : Ident.t; t1 : t
        ; x2    : Ident.t; t2 : t
        }
    | App of pc * t
    | Force of p

  and pc = { loc : Loc.t option; ty : (Type.t * Type.t) option; node : pc_node }
  and pc_node =
    | Lam of { x : Ident.t; tp : Type.t; body : t }

  and p = { loc : Loc.t option; ty : Type.t option; node : p_node }
  and p_node =
    | Suspend of t

  let t_of_node n : t = {loc = None; ty = None; node = n}
  let pc_of_node n : pc = { loc = None; ty = None; node = n}
  let p_of_node n : p = { loc = None; ty = None; node = n }
  
  let rec string_of_t e = 
    match e.node with
    | Var x -> "Var(" ^ Ident.string_of_t x ^ ")"
    | Let { x; expr; body } ->
        "Let(" ^ string_of_t expr ^ ", " ^ Ident.string_of_t x ^ ", " ^ string_of_t body ^ ")"
    | LExpr le -> "LExpr(" ^ LambdaC.Expr.string_of_t le ^ ")"
    | Phase (a, t) -> "Phase(" ^ LambdaC.Expr.string_of_t a ^ ", " ^ string_of_t t ^ ")"
    | Prod (t1, t2) -> "Prod(" ^ string_of_t t1 ^ ", " ^ string_of_t t2 ^ ")"
    | Pow (t, a) -> "Pow(" ^ string_of_t t ^ ", " ^ LambdaC.Expr.string_of_t a ^ ")"
    | CasePauli { scrut; tx; tz } -> "CasePauli(" ^ string_of_t scrut ^ ", " ^ string_of_t tx ^ ", " ^ string_of_t tz ^ ")"
    | In1 { v; _ } -> "In1(" ^ string_of_t v ^ ")"
    | In2 { v; _ } -> "In2(" ^ string_of_t v ^ ")"
    | CasePTensor { scrut; x1; t1; x2; t2 } ->
        "CasePTensor(" ^ string_of_t scrut ^ ", " ^ Ident.string_of_t x1 ^ ", " ^ string_of_t t1 ^ ", " ^ Ident.string_of_t x2 ^ ", " ^ string_of_t t2 ^ ")"
    | App (f, expr) -> string_of_pc f ^ " @ " ^ string_of_t expr
    | Force p -> string_of_p p
  
  and string_of_pc f = 
    match f.node with
    | Lam { x; tp; body } -> "lambda " ^ Ident.string_of_t x ^ " : " ^ Type.string_of_t tp ^ ". " ^ string_of_t body
  
  and string_of_p p = 
    match p.node with
    | Suspend expr -> string_of_t expr

  let rec pretty_string_of_t e = 
    match e.node with
    | Var x -> "x" ^ Ident.string_of_t x
    | Let { x; expr; body } -> 
        "let " ^ Ident.string_of_t x ^ " = " ^ pretty_string_of_t expr ^ " in " ^ pretty_string_of_t body
    | LExpr le ->
        LambdaC.Expr.pretty_string_of_t le
    | Phase (a, t) -> "<" ^ LambdaC.Expr.pretty_string_of_t a ^ "> " ^ pretty_string_of_t t
    | Prod (t1, t2) -> "(" ^ pretty_string_of_t t1 ^ ") * (" ^ pretty_string_of_t t2 ^ ")"
    | Pow (t, a) -> "(" ^ pretty_string_of_t t ^ ")^(" ^ LambdaC.Expr.pretty_string_of_t a ^ ")"
    | CasePauli { scrut; tx; tz } -> 
        "case " ^ pretty_string_of_t scrut ^ " of { X -> " ^ pretty_string_of_t tx ^ " | Z -> " ^ pretty_string_of_t tz ^ "}"
    | In1 { v; _ } -> "in1(" ^ pretty_string_of_t v ^ ")"
    | In2 { v; _ } -> "in2(" ^ pretty_string_of_t v ^ ")"
    | CasePTensor { scrut; x1; t1; x2; t2 } ->
        "case " ^ pretty_string_of_t scrut
                ^ " of { in1 x" ^  Ident.string_of_t x1 ^ " -> " ^ pretty_string_of_t t1 
                ^ " | in2 x" ^ Ident.string_of_t x2 ^ " -> " ^ pretty_string_of_t t2 ^ "}"
    | App (f, expr) -> "(" ^ pretty_string_of_pc f ^ ") @ (" ^ pretty_string_of_t expr ^ ")"
    | Force p -> pretty_string_of_p p
  
  and pretty_string_of_pc f = 
    match f.node with
    | Lam { x; tp; body } -> "lambda x" ^ Ident.string_of_t x ^ " : " ^ Type.string_of_t tp ^ ". " ^ pretty_string_of_t body
  
  and pretty_string_of_p p = 
    match p.node with
    | Suspend expr -> pretty_string_of_t expr

  let rec rename_var (from : Ident.t) (to_ : Ident.t) e =
      let node = match e.node with
      | Var x ->
          if x = from then Var to_ else Var x
      | Let { x; expr; body } ->
          let expr' = rename_var from to_ expr in
          let body' = if x = from then body else rename_var from to_ body in
          Let { x; expr = expr'; body = body' }
      | LExpr le ->
          LExpr (LambdaC.Expr.rename_var from to_ le)
      | Phase (a, t) ->
          Phase (a, rename_var from to_ t)
      | Prod (t1, t2) ->
          Prod (rename_var from to_ t1, rename_var from to_ t2)
      | Pow (t, a) ->
          Pow (rename_var from to_ t, LambdaC.Expr.rename_var from to_ a)
      | CasePauli { scrut; tx; tz } ->
          CasePauli { scrut = rename_var from to_ scrut; tx = rename_var from to_ tx; tz = rename_var from to_ tz }
      | In1 { v; tp } ->
          In1 { v = rename_var from to_ v; tp }
      | In2 { v; tp } ->
          In2 { v = rename_var from to_ v; tp }
      | CasePTensor { scrut; x1; t1; x2; t2 } ->
          let t1' = if x1 = from then t1 else rename_var from to_ t1 in
          let t2' = if x2 = from then t2 else rename_var from to_ t2 in
          CasePTensor { scrut = rename_var from to_ scrut; x1; t1 = t1'; x2; t2 = t2' }
      | App (f, expr) -> App (f, rename_var from to_ expr)
      | Force p -> Force p
      in
      { e with node }

end

module Val = struct
  type t = { phase : int; value : LambdaC.Val.t }

  let string_of_t (cfg : t) : string =
    "<" ^ string_of_int cfg.phase ^ "> " ^ LambdaC.Val.pretty_string_of_t cfg.value

  let pure (v : LambdaC.Val.t) = { phase = 0; value = v }

end



module HOAS = struct
  let var (x : Ident.t) : Expr.t = Expr.(t_of_node (Var x))
  let letin e f : Expr.t =
    let x = Ident.fresh() in
    Expr.(t_of_node (Let {x; expr=e; body=f (var x)}))
  let vec (a : LambdaC.Expr.t) : Expr.t = Expr.(t_of_node (LExpr a))
  let phase a e : Expr.t = Expr.(t_of_node (Phase(a, e)))
  let ( * ) e1 e2 : Expr.t = Expr.(t_of_node (Prod(e1,e2)))
  let pow e a : Expr.t = Expr.(t_of_node @@ Pow(e,a))
  let caseofP e ex ez : Expr.t = Expr.(t_of_node @@ CasePauli{scrut=e;tx=ex;tz=ez})
  let in1 e tp2 : Expr.t = Expr.(t_of_node @@ In1 {tp = tp2; v=e})
  let in2 tp1 e : Expr.t = Expr.(t_of_node @@ In2 {tp=tp1; v=e})
  let caseof e b1 b2 : Expr.t =
    let x1 = Ident.fresh() in
    let x2 = Ident.fresh() in
    Expr.(t_of_node @@ CasePTensor{scrut=e; x1; t1=b1 (var x1); x2; t2=b2 (var x2)})

  let lambda tp (f : Expr.t -> Expr.t) : Expr.pc =
      let x = Ident.fresh() in
      Expr.(pc_of_node @@ Lam {x; tp; body=f (var x)})
  let (@) e1 e2 : Expr.t = Expr.(t_of_node @@ App (e1, e2))

  let suspend e : Expr.p = Expr.(p_of_node @@ Suspend e)
  let force e : Expr.t = Expr.(t_of_node @@ Expr.Force e)
end

module SymplecticForm = struct
  open LambdaC.HOAS

  let ccaseP e e1 e2 =
    case e (fun x -> x * e1) (fun z -> z * e2)

  (* Assumption: the LambdaC.HOAS environment is already aware of all the variables (free or bound) in t *)
  let rec psi_of (t : Expr.t) : LambdaC.Expr.t =
    match t.node with
    | Var x -> var x
    | Let { x; expr = t1; body = t2 } -> 
        LambdaC.Expr.(t_of_node @@ Let { x; a = psi_of t1; body = psi_of t2 })
    | LExpr a -> a
    | Phase (_, t') -> psi_of t'
    | Prod (t1, t2) -> psi_of t1 + psi_of t2
    | Pow (t', a) -> a * psi_of t'
    | CasePauli { scrut; tx; tz } ->
        ccaseP (psi_of scrut)
          (psi_of tx)
          (psi_of tz)
    | In1 { v; tp } -> 
        LambdaC.Expr.(t_of_node @@ Pair (psi_of v, zero ~ty:(Type.ltype_of_t tp) ()))
    | In2 { v; tp } -> 
        LambdaC.Expr.(t_of_node @@ Pair (zero ~ty:(Type.ltype_of_t tp) (), psi_of v))
    | CasePTensor { scrut; x1; t1; x2; t2 } ->
        LambdaC.Expr.(t_of_node @@ Case { scrut = psi_of scrut; x1; a1 = psi_of t1; x2; a2 = psi_of t2 })
    | App (pc1, t2) -> psi_of_pc pc1 @ psi_of t2
    | Force p -> (match p.node with Suspend t' -> psi_of t')
  
  and psi_of_pc pc =
    match pc.node with
    | Lam { x; tp; body } -> 
        LambdaC.Expr.(t_of_node @@ Lambda { x; tp = Type.ltype_of_t tp; body = psi_of body })

  let omega1 a1 a2 =
    ccaseP a1
      (ccaseP a2 
        (*t1=[1,0],t2=[1,0]*) (const 0)
        (*t1=[1,0],t2=[0,1]*) (const (-1))
      )
      (ccaseP a2 
        (*t1=[0,1],t2=[1,0]*) (const 1)
        (*t1=[0,1],t2=[0,1]*) (const 0)
      )
  let rec omega (tp : Type.t) (a1 : LambdaC.Expr.t) (a2 : LambdaC.Expr.t) : LambdaC.Expr.t =
    match tp.node with
    | Type.Pauli -> omega1 a1 a2
    | Type.PTensor (tp1,tp2) ->
        case a1
          (fun a11 ->
            case a2
              (fun a21 -> omega tp1 a11 a21)
              (fun _ -> zero ~ty:LambdaC.HOAS.u ())
          )
          (fun a12 ->
            case a2
              (fun _ -> zero ~ty:LambdaC.HOAS.u ())
              (fun a22 -> omega tp2 a12 a22)
          )
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
    match e.node with
    | Var x -> 
      Val.pure (try VariableMap.find x ctx with Not_found -> failwith "Unbound variable")
    | Let { x; expr = e1; body = e2 } ->
      let {phase = r1; value = v1} = eval ctx e1 in
      let ctx' = VariableMap.add x v1 ctx in
      add_phase r1 (eval ctx' e2)
    | LExpr e' -> Val.pure (LEval.eval ctx e')
    | Phase (a, e') ->
      (match LEval.eval ctx a with
      | Const r -> add_phase r (eval ctx e')
      | _ -> failwith "Expected phase to evaluate to a scalar"
      )
    | Prod (e1, e2) ->
      cfg_prod (eval ctx e1) (eval ctx e2)

    | Pow (e', a) ->
      (match LEval.eval ctx a with
      | Const r -> cfg_pow (eval ctx e') r
      | _ -> failwith "Expected power to evaluate to a scalar"
      )
      

    | CasePauli { scrut; tx; tz } -> 
      (match eval ctx scrut with
      | {phase = r; value = LambdaC.Val.Pair (LambdaC.Val.Const rx, LambdaC.Val.Const rz)} ->
        let cfgx = eval ctx (HOAS.pow tx (LambdaC.HOAS.const rx)) in
        let cfgz = eval ctx (HOAS.pow tz (LambdaC.HOAS.const rz)) in
        let k    = case_phase rx rz in
        add_phase (r + k) (cfg_prod cfgz cfgx)

      | _ -> failwith "eval [CasePauli] -> expected a value of Pauli type")

    | In1 { v; tp } ->
      let {phase = r; value = v'} = eval ctx v in
      let v'' = LambdaC.Val.Pair (v', LEval.vzero (Type.ltype_of_t tp)) in
      {phase = r; value = v''}
    | In2 { v; tp } ->
      let {phase = r; value = v'} = eval ctx v in
      let v'' = LambdaC.Val.Pair (LEval.vzero (Type.ltype_of_t tp), v') in
      {phase = r; value = v''}

    | CasePTensor { scrut; x1; t1; x2; t2 } ->
      (match eval ctx scrut with
      | {phase = r; value = LambdaC.Val.Pair (v1,v2)} ->
        let ctx1 = VariableMap.add x1 v1 ctx in
        let cfg1 = eval ctx1 t1 in
        let ctx2 = VariableMap.add x2 v2 ctx in
        let cfg2 = eval ctx2 t2 in
        let cfg = cfg_prod cfg1 cfg2 in
        add_phase r cfg

      | _ -> failwith "eval [CasePTensor] -> expected a pair")

    | App (pc1, e2) ->
        let {phase = r; value = v} = eval ctx e2 in
        (match pc1.node with
        | Lam { x; body; _ } ->
          let ctx' = VariableMap.singleton x v in
          add_phase r (eval ctx' body))
    | Force p -> 
        (match p.node with 
        | Suspend e' -> eval (VariableMap.empty) e')

  let evalClosed e = eval VariableMap.empty e

end