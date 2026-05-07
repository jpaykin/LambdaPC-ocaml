open Ident
 
module Type = struct


  type t = { loc : Loc.t option; node : node }
  and node =
      | Unit
      | Sum of t * t
      | Arrow of t * t

  let t_of_node n = { loc = None ; node = n }

  let rec string_of_t tp =
          match tp.node with
          | Unit -> "Unit"
          | Sum (t1, t2) -> "Sum(" ^ string_of_t t1 ^ ", " ^ string_of_t t2 ^ ")"
          | Arrow (t1, t2) -> "Arrow(" ^ string_of_t t1 ^ " -> " ^ string_of_t t2 ^ ")"

  let rec pretty tp =
      match tp.node with
      | Unit -> "Zd"
      | Sum (t1, t2) -> "(" ^ pretty t1 ^ " + " ^ pretty t2 ^ ")"
      | Arrow (t1, t2) -> "(" ^ pretty t1 ^ " -o " ^ pretty t2 ^ ")"

end


module Expr = struct

  type t = { loc : Loc.t option; ty : Type.t option; node : node }
  and node =
    | Var of Ident.t
    | Zero
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
    | Let of { x : Ident.t; a : t; body : t }

    let t_of_node ?(loc=None) ?(ty=None) (n : node) : t =
        { loc ; ty ; node = n }

    let rec string_of_t a =
      match a.node with
      | Var x -> "Var(" ^ Ident.string_of_t x ^ ")"
      | Let { x; a = a1; body = a2 } -> "Let(" ^ string_of_t a1 ^ ", " ^ Ident.string_of_t x ^ ", " ^ string_of_t a2 ^ ")"
      | Zero -> "Zero"
      | Plus (a1, a2) -> "Plus(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Const c -> "Const(" ^ string_of_int c ^ ")"
      | Scale (a1, a2) -> "Scale(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Pair (a1, a2) -> "Pair(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"
      | Case { scrut; x1; a1; x2; a2 } ->
          "Case(" ^ string_of_t scrut ^ ", " ^ Ident.string_of_t x1 ^ ", " ^ string_of_t a1 ^ ", " ^ Ident.string_of_t x2 ^ ", " ^ string_of_t a2 ^ ")"
      | Lambda { x; tp; body } ->
          "Lambda(" ^ Ident.string_of_t x ^ ":" ^ Type.string_of_t tp ^ ". " ^ string_of_t body ^ ")"
      | App (a1, a2) -> "App(" ^ string_of_t a1 ^ ", " ^ string_of_t a2 ^ ")"

    
    let rec pretty_string_of_t a =
      match a.node with
      | Var x -> "x" ^ Ident.string_of_t x
      | Let { x; a = a1; body = a2 } -> "let x" ^ Ident.string_of_t x ^ " = " ^ pretty_string_of_t a1 ^ " in " ^ pretty_string_of_t a2
      | Zero -> "0"
      | Plus (a1, a2) -> pretty_string_of_t a1 ^ " + " ^ pretty_string_of_t a2
      | Const c -> string_of_int c
      | Scale (a1, a2) -> pretty_string_of_t a1 ^ " * " ^ pretty_string_of_t a2
      | Pair (a1, a2) -> 
          (match a1.node, a2.node with
           | Const 0, Const 0 -> "I"
           | Const 1, Const 0 -> "X"
           | Const 0, Const 1 -> "Z"
           | Const 1, Const 1 -> "Y"
           | _, _ -> "[" ^ pretty_string_of_t a1 ^ ", " ^ pretty_string_of_t a2 ^ "]")
      | Case { scrut; x1; a1; x2; a2 } ->
          "case " ^ pretty_string_of_t scrut
          ^ " of { x" ^ Ident.string_of_t x1 ^ " -> " ^ pretty_string_of_t a1 
          ^ " | x" ^ Ident.string_of_t x2 ^ " -> " ^ pretty_string_of_t a2 ^ "}"
      | Lambda { x; tp; body } ->
          "lambda x" ^ Ident.string_of_t x ^ ":" ^ Type.string_of_t tp ^ ". " ^ pretty_string_of_t body
      | App (a1, a2) -> pretty_string_of_t a1 ^ " @ " ^ pretty_string_of_t a2


    (* NOT epxlicitly capture-avoiding, assumes NO reuse of variables *)
    let rec subst (from : Ident.t) (to_ : t) (a : t) : t =
      let result_node = 
        match a.node with
        | Var x -> if x = from then to_.node else Var x
        | Let { x; a = a1; body = a2 } ->
          Let { x;
                a = subst from to_ a1;
                body = if x = from then a2 else subst from to_ a2 }
        | Zero -> Zero
        | Const c -> Const c
        | Plus (a1, a2) -> Plus (subst from to_ a1, subst from to_ a2)
        | Scale (a1, a2) -> Scale (subst from to_ a1, subst from to_ a2)
        | Pair (a1, a2) -> Pair (subst from to_ a1, subst from to_ a2)
        | Case { scrut; x1; a1; x2; a2 } ->
            let a1' = if x1 = from then a1 else subst from to_ a1 in
            let a2' = if x2 = from then a2 else subst from to_ a2 in
            Case { scrut = subst from to_ scrut; x1; a1 = a1'; x2; a2 = a2' }
        | Lambda { x; tp; body } ->
            if x = from then Lambda { x; tp; body }
            else Lambda { x; tp; body = subst from to_ body }
        | App (a1, a2) -> App (subst from to_ a1, subst from to_ a2)
      in
      { a with node = result_node }
    let rename_var x y e = subst x { e with node = Var y } e

    (* Apply the function f to all constants in a *)
    let rec map (f : int -> int) (a : t) : t =
      let result_node =
        match a.node with
        | Var x -> Var x
        | Let { x; a = a1; body = a2 } -> Let { x; a = map f a1; body = map f a2 }
        | Zero -> Zero
        | Const c -> Const (f c)
        | Plus (a1, a2) -> Plus (map f a1, map f a2)
        | Scale (a1, a2) -> Scale (map f a1, map f a2)
        | Pair (a1, a2) -> Pair (map f a1, map f a2)
        | Case { scrut; x1; a1; x2; a2 } ->
          Case { scrut = map f scrut; x1; a1 = map f a1; x2; a2 = map f a2 }
        | Lambda { x; tp; body } -> Lambda { x; tp; body = map f body }
        | App (a1, a2) -> App (map f a1, map f a2)
      in
      { a with node = result_node }


    (* Update env so its next fresh variable is not in a *)
    let rec update_env a =
      match a.node with
      | Var x -> Ident.seed x
      | Let { x; a = a1; body = a2 } ->
        Ident.seed x;
        update_env a1;
        update_env a2
      | Zero -> ()
      | Const _ -> ()
      | Plus (a1, a2) -> update_env a1; update_env a2
      | Scale (a1, a2) -> update_env a1; update_env a2
      | Pair (a1, a2) -> update_env a1; update_env a2
      | Case { scrut; x1; a1; x2; a2 } ->
        update_env scrut;
        Ident.seed x1;
        Ident.seed x2;
        update_env a1;
        update_env a2
      | Lambda { x; body; _ } ->
        Ident.seed x;
        update_env body
      | App (a1, a2) -> update_env a1; update_env a2


    let bind_binders env_lr env_rl (xl : Ident.t) (xr : Ident.t) =
      let l = VariableMap.find_opt xl env_lr in
      let r = VariableMap.find_opt xr env_rl in
      match l, r with
      | None, None ->
          Some (
            VariableMap.add xl xr env_lr,
            VariableMap.add xr xl env_rl
          )
      | Some xr', Some xl' when Ident.equal xr xr' && Ident.equal xl xl' ->
          Some (env_lr, env_rl)
      | _, _ ->
          None

    (* alpha equivalence with explicit binder correspondence; does not allocate fresh ids *)
    let rec alpha_equiv' env_lr env_rl a1 a2 =
      match a1.node, a2.node with
      | Var x1, Var x2 ->
        (match VariableMap.find_opt x1 env_lr, VariableMap.find_opt x2 env_rl with
         | Some x2', Some x1' -> Ident.equal x2 x2' && Ident.equal x1 x1'
         | None, None -> Ident.equal x1 x2
         | _, _ -> false)
      | Let { x = x1; a = a11; body = a12 }, Let { x = x2; a = a21; body = a22 } ->
        alpha_equiv' env_lr env_rl a11 a21
        &&
        (match bind_binders env_lr env_rl x1 x2 with
         | Some (env_lr', env_rl') -> alpha_equiv' env_lr' env_rl' a12 a22
         | None -> false)
      | Zero, Zero -> true
      | Plus (a11, a12), Plus (a21, a22)
      | Scale (a11, a12), Scale (a21, a22)
      | Pair (a11, a12), Pair (a21, a22)
      | App (a11, a12), App (a21, a22) ->
        alpha_equiv' env_lr env_rl a11 a21 && alpha_equiv' env_lr env_rl a12 a22
      | Const v1, Const v2 ->
        v1 = v2
      | Case { scrut = scrut1; x1 = x11; a1 = a11; x2 = x12; a2 = a12 }, 
        Case { scrut = scrut2; x1 = x21; a1 = a21; x2 = x22; a2 = a22 } ->
        alpha_equiv' env_lr env_rl scrut1 scrut2
        &&
        (match bind_binders env_lr env_rl x11 x21, bind_binders env_lr env_rl x12 x22 with
         | Some (env_lr1, env_rl1), Some (env_lr2, env_rl2) ->
           alpha_equiv' env_lr1 env_rl1 a11 a21
           && alpha_equiv' env_lr2 env_rl2 a12 a22
         | _, _ -> false)
      | Lambda { x = x1; tp = tp1; body = body1 }, Lambda { x = x2; tp = tp2; body = body2 } ->
        tp1 = tp2
        &&
        (match bind_binders env_lr env_rl x1 x2 with
         | Some (env_lr', env_rl') -> alpha_equiv' env_lr' env_rl' body1 body2
         | None -> false)
      | _, _ -> false

    let alpha_equiv a1 a2 =
      alpha_equiv' VariableMap.empty VariableMap.empty a1 a2

  end
