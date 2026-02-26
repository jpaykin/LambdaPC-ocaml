open Named_ast

exception Error of { loc : Loc.t; msg : string }

type env = (Symbol.t * Ident.t) list

(* Does our symbol exist in the env? *)
let rec lookup (env : env) (key : Symbol.t) : Ident.t option =
  match env with
  | [] -> None
  | (sym, id) :: tl -> if Symbol.equal sym key then Some id else lookup tl key

let freshen (x : Ident.t) : Ident.t =
  let fresh = Symbol.gensym ~hint:x.text () in
  { x with sym = fresh; text = Symbol.name fresh }

(* Keep location information but rewrite name/text *)
let rewrite_var (binder : Ident.t) (site : Ident.t) : Ident.t =
  { site with sym = binder.sym; text = binder.text }

let rec resolve_c (env : env) (e : LambdaC_Surface.expr) : LambdaC_Surface.expr =
  let loc = e.loc in
  let mk node = { e with node; loc } in
  match e.node with
  | Var x -> (
      match lookup env x.sym with
      | Some fresh_x -> mk (Var (rewrite_var fresh_x x))
      | None ->
          raise (Error { loc = x.loc; msg = "unbound LambdaC variable " ^ x.text })
    )

  | Zero tp -> mk (Zero tp)
  | Const n -> mk (Const n)

  | Plus (a, b) ->
      mk (Plus (resolve_c env a, resolve_c env b))

  | Scale (a, b) ->
      mk (Scale (resolve_c env a, resolve_c env b))

  | Pair (a, b) ->
      mk (Pair (resolve_c env a, resolve_c env b))

  | App (f, x) ->
      mk (App (resolve_c env f, resolve_c env x))

  | Annot (a, tp) ->
      mk (Annot (resolve_c env a, tp))

  | Let { x; rhs; body } ->
      let rhs' = resolve_c env rhs in
      let x' = freshen x in
      let env' = (x.sym, x') :: env in
      let body' = resolve_c env' body in
      mk (Let { x = x'; rhs = rhs'; body = body' })

  | Lambda { x; tp; body } ->
      let x' = freshen x in
      let env' = (x.sym, x') :: env in
      let body' = resolve_c env' body in
      mk (Lambda { x = x'; tp; body = body' })

  | Case { scrut; x1; a1; x2; a2 } ->
      let scrut' = resolve_c env scrut in

      let x1' = freshen x1 in
      let a1' = resolve_c ((x1.sym, x1') :: env) a1 in

      let x2' = freshen x2 in
      let a2' = resolve_c ((x2.sym, x2') :: env) a2 in

      mk (Case { scrut = scrut'; x1 = x1'; a1 = a1'; x2 = x2'; a2 = a2' })

let resolve_c_top (e : LambdaC_Surface.expr) : LambdaC_Surface.expr =
  resolve_c [] e

let rec resolve_pc (env_pc : env) (env_c : env) (e : LambdaPC_Surface.expr)
  : LambdaPC_Surface.expr =
  let loc = e.loc in
  let mk node = { e with node; loc } in
  match e.node with
  | Var x -> (
      match lookup env_pc x.sym with
      | Some fresh_x -> mk (Var (rewrite_var fresh_x x))
      | None ->
          raise (Error { loc = x.loc; msg = "unbound LambdaPC variable " ^ x.text })
    )

  | Let { x; rhs; body } ->
      let rhs' = resolve_pc env_pc env_c rhs in
      let x' = freshen x in
      let env_pc' = (x.sym, x') :: env_pc in
      let body' = resolve_pc env_pc' env_c body in
      mk (Let { x = x'; rhs = rhs'; body = body' })

  | LExpr le ->
      mk (LExpr (resolve_c env_c le))

  | Phase (le, t) ->
      mk (Phase (resolve_c env_c le, resolve_pc env_pc env_c t))

  | Prod (a, b) ->
      mk (Prod (resolve_pc env_pc env_c a, resolve_pc env_pc env_c b))

  | Pow (t, le) ->
      mk (Pow (resolve_pc env_pc env_c t, resolve_c env_c le))

  | CasePauli { scrut; tx; tz } ->
      mk (CasePauli
            { scrut = resolve_pc env_pc env_c scrut
            ; tx = resolve_pc env_pc env_c tx
            ; tz = resolve_pc env_pc env_c tz
            })

  | In1 { tp; v } ->
      mk (In1 { tp; v = resolve_pc env_pc env_c v })

  | In2 { tp; v } ->
      mk (In2 { tp; v = resolve_pc env_pc env_c v })

  | CasePTensor { scrut; x1; t1; x2; t2 } ->
      let scrut' = resolve_pc env_pc env_c scrut in
      let x1' = freshen x1 in
      let env_pc1 = (x1.sym, x1') :: env_pc in
      let t1' = resolve_pc env_pc1 env_c t1 in

      let x2' = freshen x2 in
      let env_pc2 = (x2.sym, x2') :: env_pc in
      let t2' = resolve_pc env_pc2 env_c t2 in

      mk (CasePTensor { scrut = scrut'; x1 = x1'; t1 = t1'; x2 = x2'; t2 = t2' })

  | Apply (f, arg) ->
      mk (Apply (resolve_pc_fun env_pc env_c f, resolve_pc env_pc env_c arg))

  | Force p ->
      mk (Force (resolve_p env_pc env_c p))

and resolve_pc_fun (env_pc : env) (env_c : env) (f : LambdaPC_Surface.pc)
  : LambdaPC_Surface.pc =
  let loc = f.loc in
  match f.node with
  | Lam { x; tp; body } ->
      let x' = freshen x in
      let env_pc' = (x.sym, x') :: env_pc in
      let body' = resolve_pc env_pc' env_c body in
      { loc; node = Lam { x = x'; tp; body = body' } }

and resolve_p (env_pc : env) (env_c : env) (p : LambdaPC_Surface.p)
  : LambdaPC_Surface.p =
  let loc = p.loc in
  match p.node with
  | Suspend e ->
      { loc; node = Suspend (resolve_pc env_pc env_c e) }

let resolve_pc_top (e : LambdaPC_Surface.expr) : LambdaPC_Surface.expr =
  resolve_pc [] [] e
