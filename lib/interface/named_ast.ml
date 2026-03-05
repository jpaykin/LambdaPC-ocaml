open Ident

module LambdaC_Surface = struct
  module Type = struct
    type t = { loc : Loc.t; node : node }
    and node =
      | Unit
      | Sum of t * t
      | Arrow of t * t

    let rec pretty_string_of_t (tp : t) : string =
      match tp.node with
      | Unit -> "Zd"
      | Sum (tp1, tp2) ->
          "(" ^ pretty_string_of_t tp1 ^ " + " ^ pretty_string_of_t tp2 ^ ")"
      | Arrow (tp1, tp2) ->
          "(" ^ pretty_string_of_t tp1 ^ " -o " ^ pretty_string_of_t tp2 ^ ")"
  end

  type expr = { loc : Loc.t; ty : Type.t option; node : node }
  and node =
    | Var of Ident.t
    | Const of int
    | Plus of expr * expr
    | Scale of expr * expr
    | Pair of expr * expr
    | Case of
        { scrut : expr
        ; x1 : Ident.t
        ; a1 : expr
        ; x2 : Ident.t
        ; a2 : expr
        }
    | Lambda of { x : Ident.t; tp : Type.t; body : expr }
    | App of expr * expr
    | Let of { x : Ident.t; rhs : expr; body : expr }
    | Annot of expr * Type.t

  let pretty_string_of_ident (x : Ident.t) : string = x.text

  let rec pretty_string_of_expr (e : expr) : string =
    match e.node with
    | Var x -> "var " ^ pretty_string_of_ident x
    | Const n -> string_of_int n
    | Plus (e1, e2) -> pretty_string_of_expr e1 ^ " + " ^ pretty_string_of_expr e2
    | Scale (e1, e2) -> pretty_string_of_expr e1 ^ " .* " ^ pretty_string_of_expr e2
    | Pair (e1, e2) ->
        "[" ^ pretty_string_of_expr e1 ^ ", " ^ pretty_string_of_expr e2 ^ "]"
    | Case { scrut; x1; a1; x2; a2 } ->
        "case " ^ pretty_string_of_expr scrut ^ " of { in1 "
        ^ pretty_string_of_ident x1 ^ " -> " ^ pretty_string_of_expr a1
        ^ " | in2 " ^ pretty_string_of_ident x2 ^ " -> " ^ pretty_string_of_expr a2
        ^ " }"
    | Lambda { x; tp; body } ->
        "lambda " ^ pretty_string_of_ident x ^ " : " ^ Type.pretty_string_of_t tp
        ^ ". " ^ pretty_string_of_expr body
    | App (f, arg) ->
        pretty_string_of_expr f ^ " .@ " ^ pretty_string_of_expr arg
    | Let { x; rhs; body } ->
        "let " ^ pretty_string_of_ident x ^ " = " ^ pretty_string_of_expr rhs
        ^ " in " ^ pretty_string_of_expr body
    | Annot (e', tp) ->
        "(" ^ pretty_string_of_expr e' ^ " : " ^ Type.pretty_string_of_t tp ^ ")"
end

module LambdaPC_Surface = struct
  module Type = struct
    type t = { loc : Loc.t; node : node }
    and node =
      | Pauli
      | PTensor of t * t

    let rec pretty_string_of_t (tp : t) : string =
      match tp.node with
      | Pauli -> "Pauli"
      | PTensor (tp1, tp2) ->
          "(" ^ pretty_string_of_t tp1 ^ " ** " ^ pretty_string_of_t tp2 ^ ")"
  end

  type expr = { loc : Loc.t; ty : Type.t option; node : node }
  and node =
    | Var   of Ident.t
    | Let   of { x : Ident.t; rhs : expr; body : expr }
    | LExpr of LambdaC_Surface.expr
    | Phase of LambdaC_Surface.expr * expr
    | Prod  of expr * expr
    | Pow   of expr * LambdaC_Surface.expr
    | CasePauli   of { scrut : expr; tx : expr; tz : expr }
    | In1 of { tp : Type.t; v : expr }
    | In2 of { tp : Type.t; v : expr }
    | CasePTensor of
        { scrut : expr
        ; x1    : Ident.t; t1 : expr
        ; x2    : Ident.t; t2 : expr
        }
    | Apply of pc * expr
    | Force of p

  and pc = { loc : Loc.t; node : pc_node }
  and pc_node =
    | Lam of { x : Ident.t; tp : Type.t; body : expr }

  and p = { loc : Loc.t; node : p_node }
  and p_node =
    | Suspend of expr

  let pretty_string_of_ident (x : Ident.t) : string = x.text

  let rec pretty_string_of_expr (e : expr) : string =
    match e.node with
    | Var x -> pretty_string_of_ident x
    | Let { x; rhs; body } ->
        "let " ^ pretty_string_of_ident x ^ " = " ^ pretty_string_of_expr rhs
        ^ " in " ^ pretty_string_of_expr body
    | LExpr a -> LambdaC_Surface.pretty_string_of_expr a
    | Phase (a, t) ->
        "<" ^ LambdaC_Surface.pretty_string_of_expr a ^ "> " ^ pretty_string_of_expr t
    | Prod (t1, t2) -> "(" ^ pretty_string_of_expr t1 ^ ") * (" ^ pretty_string_of_expr t2 ^ ")"
    | Pow (t, a) ->
        "(" ^ pretty_string_of_expr t ^ ")^{" ^ LambdaC_Surface.pretty_string_of_expr a ^ "}"
    | CasePauli { scrut; tx; tz } ->
        "case " ^ pretty_string_of_expr scrut ^ " of { X -> "
        ^ pretty_string_of_expr tx ^ " | Z -> " ^ pretty_string_of_expr tz ^ " }"
    | In1 { tp; v } ->
        "in1 " ^ Type.pretty_string_of_t tp ^ " " ^ pretty_string_of_expr v
    | In2 { tp; v } ->
        "in2 " ^ Type.pretty_string_of_t tp ^ " " ^ pretty_string_of_expr v
    | CasePTensor { scrut; x1; t1; x2; t2 } ->
        "case " ^ pretty_string_of_expr scrut ^ " of { in1 "
        ^ pretty_string_of_ident x1 ^ " -> " ^ pretty_string_of_expr t1
        ^ " | in2 " ^ pretty_string_of_ident x2 ^ " -> " ^ pretty_string_of_expr t2
        ^ " }"
    | Apply (f, t) ->
        "(" ^ pretty_string_of_pc f ^ ") @ (" ^ pretty_string_of_expr t ^ ")"
    | Force p -> pretty_string_of_p p
  and pretty_string_of_pc (f : pc) : string =
    match f.node with
    | Lam { x; tp; body } ->
        "lambda " ^ pretty_string_of_ident x ^ " : " ^ Type.pretty_string_of_t tp
        ^ ". " ^ pretty_string_of_expr body
  and pretty_string_of_p (p : p) : string =
    match p.node with
    | Suspend t -> "suspend " ^ pretty_string_of_expr t
end
