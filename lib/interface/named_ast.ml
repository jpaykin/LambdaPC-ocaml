module Loc = struct
  type t = { sp : Lexing.position; ep : Lexing.position}

  let mk sp ep = { sp; ep }
end

module Ident = struct
  type t =
    { text : string
    ; sym : Symbol.t
    ; loc : Loc.t
    }

  let mk ~text ~loc = { text; sym = Symbol.intern text; loc }
end

module LambdaC_Surface = struct
  module Type = struct
    type t = { loc : Loc.t; node : node }
    and node =
      | Unit
      | Sum of t * t
      | Arrow of t * t
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
end

module LambdaPC_Surface = struct
  module Type = struct
    type t = { loc : Loc.t; node : node }
    and node =
      | Pauli
      | PTensor of t * t
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
end


