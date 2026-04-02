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
    | Zero of Type.t
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

  let rec iter_binders (f : Ident.t -> unit) (e : expr) : unit =
    match e.node with
    | Var _ | Zero _ | Const _ -> ()
    | Plus (e1, e2)
    | Scale (e1, e2)
    | Pair (e1, e2)
    | App (e1, e2) ->
        iter_binders f e1;
        iter_binders f e2
    | Case { scrut; x1; a1; x2; a2 } ->
        iter_binders f scrut;
        f x1;
        iter_binders f a1;
        f x2;
        iter_binders f a2
    | Lambda { x; body; _ } ->
        f x;
        iter_binders f body
    | Let { x; rhs; body } ->
        iter_binders f rhs;
        f x;
        iter_binders f body
    | Annot (e', _) ->
        iter_binders f e'

  let seed_fresh (e : expr) : unit =
    iter_binders (fun x -> Fresh.seed x.sym) e

  let rec pretty_string_of_expr (e : expr) : string =
    match e.node with
    | Var x -> "var " ^ pretty_string_of_ident x
    | Zero tp -> "zero{" ^ Type.pretty_string_of_t tp ^ "}"
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

  let rec iter_binders (f : Ident.t -> unit) (e : expr) : unit =
    match e.node with
    | Var _ -> ()
    | Let { x; rhs; body } ->
        iter_binders f rhs;
        f x;
        iter_binders f body
    | LExpr a ->
        LambdaC_Surface.iter_binders f a
    | Phase (a, t) ->
        LambdaC_Surface.iter_binders f a;
        iter_binders f t
    | Prod (t1, t2) ->
        iter_binders f t1;
        iter_binders f t2
    | Pow (t, a) ->
        iter_binders f t;
        LambdaC_Surface.iter_binders f a
    | CasePauli { scrut; tx; tz } ->
        iter_binders f scrut;
        iter_binders f tx;
        iter_binders f tz
    | In1 { v; _ } | In2 { v; _ } ->
        iter_binders f v
    | CasePTensor { scrut; x1; t1; x2; t2 } ->
        iter_binders f scrut;
        f x1;
        iter_binders f t1;
        f x2;
        iter_binders f t2
    | Apply (pc, t) ->
        iter_binders_pc f pc;
        iter_binders f t
    | Force p ->
        iter_binders_p f p

  and iter_binders_pc (f : Ident.t -> unit) (pc : pc) : unit =
    match pc.node with
    | Lam { x; body; _ } ->
        f x;
        iter_binders f body

  and iter_binders_p (f : Ident.t -> unit) (p : p) : unit =
    match p.node with
    | Suspend t -> iter_binders f t

  let seed_fresh (e : expr) : unit =
    iter_binders (fun x -> Fresh.seed x.sym) e

  let seed_fresh_pc (pc : pc) : unit =
    iter_binders_pc (fun x -> Fresh.seed x.sym) pc

  let seed_fresh_p (p : p) : unit =
    iter_binders_p (fun x -> Fresh.seed x.sym) p

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
