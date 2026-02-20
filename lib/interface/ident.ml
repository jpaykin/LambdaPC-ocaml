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
  let compare x1 x2 = Symbol.compare x1.sym x2.sym
  let equal x1 x2 = Symbol.equal x1.sym x2.sym
  let string_of_t x = Symbol.name x.sym
end
module VariableMap = Map.Make(Ident)

(* Not sure if we still need this or if it's encompassed by Symbol now *)
  (*
module VariableEnvironment = struct
  type t = Ident.t ref
  
  let init : t = {contents = 0}
  let fresh (x : t) : Ident.t =
    let y = !x in
    x := y + 1;
    y

  (* Record the existance of the variable x *)
  let update (x : Variable.t) (env : t) : unit =
    env := max (x+1) !env
end 
*)


module IdentSet = struct
  module M = Set.Make(Ident)
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

(*
module LambdaPC = struct
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
    | LExpr of LambdaC.expr
    | Phase of LambdaC.expr * expr
    | Prod  of expr * expr
    | Pow   of expr * LambdaC.expr
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


*)