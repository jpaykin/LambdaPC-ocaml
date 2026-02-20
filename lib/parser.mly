%{
open Named_ast


let loc_of sp ep : Loc.t = Loc.mk sp ep

let mk_ident (s : string) (sp : Lexing.position) (ep : Lexing.position) : Ident.t =
  Ident.mk ~text:s ~loc:(loc_of sp ep)

let mk_lty sp ep node : LambdaC.Type.t =
  { loc = loc_of sp ep; node }

let mk_pty sp ep node : LambdaPC.Type.t =
  { loc = loc_of sp ep; node }

let mk_lexpr sp ep node : LambdaC.expr =
  { loc = loc_of sp ep; ty = None; node }

let mk_expr sp ep node : LambdaPC.expr =
  { loc = loc_of sp ep; ty = None; node }

let mk_pc sp ep node : LambdaPC.pc =
  { loc = loc_of sp ep; node }

let mk_p sp ep node : LambdaPC.p =
  { loc = loc_of sp ep; node }

let mk_const_lexpr n sp ep : LambdaC.expr =
  mk_lexpr sp ep (LambdaC.Const n)
%}

%token <string> ID
%token <int> INT
%token LVARTAG

%token LET
%token IN
%token EQUALS
%token CASE
%token OF
%token ARROW
%token MID
%token LAM
%token COLON
%token DOT
%token SUSPEND
%token COMMA
%token CLIF

%token XCONST
%token ZCONST
%token YCONST
%token ICONST
%token IN1
%token IN2
%token ZERO

%token STAR
%token CARROT
%token AT

%token PAULI
%token UNIT
%token PLUS
%token LOLLI
%token TENSOR


%token LANGLE
%token RANGLE
%token LCURLY
%token RCURLY
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE

%token EOF

%nonassoc IN
%nonassoc RANGLE
%nonassoc RCURLY

%right LOLLI
%right PLUS
%right TENSOR
%right DOT
%right STAR
%right AT
%right SUSPEND
%right CARROT


%start <LambdaPC.expr> prog
%start <LambdaPC.pc> pcprog

%type <LambdaPC.expr> expr
%type <LambdaC.expr> lexpr
%type <LambdaPC.pc> pclif
%type <LambdaPC.p> pauli

%type <LambdaPC.Type.t> ptype
%type <LambdaC.Type.t> ltype

%%

(* RULES *)
prog:
  | e=expr EOF { e }

pcprog:
  | f=pclif EOF { f }

ptype:
  | PAULI
      { mk_pty $startpos $endpos LambdaPC.Type.Pauli }
  | tp1=ptype TENSOR tp2=ptype
      { mk_pty $startpos $endpos (LambdaPC.Type.PTensor (tp1, tp2)) }
  | LPAREN tp=ptype RPAREN
      { tp }

ltype:
  | UNIT
      { mk_lty $startpos $endpos LambdaC.Type.Unit }
  | tp1=ltype PLUS tp2=ltype
      { mk_lty $startpos $endpos (LambdaC.Type.Sum (tp1, tp2)) }
  | tp1=ltype LOLLI tp2=ltype
      { mk_lty $startpos $endpos (LambdaC.Type.Arrow (tp1, tp2)) }
  | LPAREN tp=ltype RPAREN
      { tp }

lexpr:
  | LVARTAG x=ID
      { let id = mk_ident x $startpos(x) $endpos(x) in
        mk_lexpr $startpos $endpos (LambdaC.Var id) }

  | ZERO LCURLY tp=ltype RCURLY
      { mk_lexpr $startpos $endpos (LambdaC.Zero tp) }

  | XCONST
      { let sp, ep = $startpos, $endpos in
        let c1 = mk_const_lexpr 1 sp ep in
        let c0 = mk_const_lexpr 0 sp ep in
        mk_lexpr sp ep (LambdaC.Pair (c1, c0)) }

  | YCONST
      { let sp, ep = $startpos, $endpos in
        let c1a = mk_const_lexpr 1 sp ep in
        let c1b = mk_const_lexpr 1 sp ep in
        mk_lexpr sp ep (LambdaC.Pair (c1a, c1b)) }

  | ZCONST
      { let sp, ep = $startpos, $endpos in
        let c0 = mk_const_lexpr 0 sp ep in
        let c1 = mk_const_lexpr 1 sp ep in
        mk_lexpr sp ep (LambdaC.Pair (c0, c1)) }

  | ICONST
      { let sp, ep = $startpos, $endpos in
        let c0a = mk_const_lexpr 0 sp ep in
        let c0b = mk_const_lexpr 0 sp ep in
        mk_lexpr sp ep (LambdaC.Pair (c0a, c0b)) }

  | a1=lexpr PLUS a2=lexpr
      { mk_lexpr $startpos $endpos (LambdaC.Plus (a1, a2)) }

  | r=INT
      { mk_lexpr $startpos $endpos (LambdaC.Const r) }

  | a1=lexpr DOT STAR a2=lexpr
      { mk_lexpr $startpos $endpos (LambdaC.Scale (a1, a2)) }

  | LSQUARE a1=lexpr COMMA a2=lexpr RSQUARE
      { mk_lexpr $startpos $endpos (LambdaC.Pair (a1, a2)) }

  | DOT CASE scrut=lexpr OF LCURLY IN1 x1=ID ARROW a1=lexpr
                        MID    IN2 x2=ID ARROW a2=lexpr RCURLY
      { let x1id = mk_ident x1 $startpos(x1) $endpos(x1) in
        let x2id = mk_ident x2 $startpos(x2) $endpos(x2) in
        mk_lexpr $startpos $endpos
          (LambdaC.Case { scrut; x1 = x1id; a1; x2 = x2id; a2 }) }

  | LAM x=ID COLON tp=ltype DOT body=lexpr
      { let xid = mk_ident x $startpos(x) $endpos(x) in
        mk_lexpr $startpos $endpos
          (LambdaC.Lambda { x = xid; tp; body }) }

  | a1=lexpr DOT AT a2=lexpr
      { mk_lexpr $startpos $endpos (LambdaC.App (a1, a2)) }

  | LPAREN a=lexpr RPAREN
      { a }

expr:
  | x=ID
      { let id = mk_ident x $startpos(x) $endpos(x) in
        mk_expr $startpos $endpos (LambdaPC.Var id) }

  | LET x=ID EQUALS rhs=expr IN body=expr
      { let xid = mk_ident x $startpos(x) $endpos(x) in
        mk_expr $startpos $endpos (LambdaPC.Let { x = xid; rhs; body }) }

  | a=lexpr
      { mk_expr $startpos $endpos (LambdaPC.LExpr a) }

  | LANGLE a=lexpr RANGLE t=expr
      { mk_expr $startpos $endpos (LambdaPC.Phase (a, t)) }

  | t1=expr STAR t2=expr
      { mk_expr $startpos $endpos (LambdaPC.Prod (t1, t2)) }

  | t=expr CARROT n=INT
      { let nlex = mk_lexpr $startpos(n) $endpos(n) (LambdaC.Const n) in
        mk_expr $startpos $endpos (LambdaPC.Pow (t, nlex)) }

  | t=expr CARROT LCURLY n=INT RCURLY
      { let nlex = mk_lexpr $startpos(n) $endpos(n) (LambdaC.Const n) in
        mk_expr $startpos $endpos (LambdaPC.Pow (t, nlex)) }

  | t=expr CARROT LCURLY a=lexpr RCURLY
      { mk_expr $startpos $endpos (LambdaPC.Pow (t, a)) }

  | CASE scrut=expr OF LCURLY XCONST ARROW tx=expr MID ZCONST ARROW tz=expr RCURLY
      { mk_expr $startpos $endpos (LambdaPC.CasePauli { scrut; tx; tz }) }

  | CASE scrut=expr OF LCURLY ZCONST ARROW tz=expr MID XCONST ARROW tx=expr RCURLY
      { mk_expr $startpos $endpos (LambdaPC.CasePauli { scrut; tx; tz }) }

  | IN1 tp=ptype v=expr
      { mk_expr $startpos $endpos (LambdaPC.In1 { tp; v }) }

  | IN2 tp=ptype v=expr
      { mk_expr $startpos $endpos (LambdaPC.In2 { tp; v }) }

  | CASE scrut=expr OF LCURLY IN1 x1=ID ARROW t1=expr
                       MID    IN2 x2=ID ARROW t2=expr RCURLY
      { let x1id = mk_ident x1 $startpos(x1) $endpos(x1) in
        let x2id = mk_ident x2 $startpos(x2) $endpos(x2) in
        mk_expr $startpos $endpos
          (LambdaPC.CasePTensor { scrut; x1 = x1id; t1; x2 = x2id; t2 }) }

  | CASE scrut=expr OF LCURLY IN2 x2=ID ARROW t2=expr
                       MID    IN1 x1=ID ARROW t1=expr RCURLY
      { let x1id = mk_ident x1 $startpos(x1) $endpos(x1) in
        let x2id = mk_ident x2 $startpos(x2) $endpos(x2) in
        mk_expr $startpos $endpos
          (LambdaPC.CasePTensor { scrut; x1 = x1id; t1; x2 = x2id; t2 }) }

  | f=pclif AT t=expr
      { mk_expr $startpos $endpos (LambdaPC.Apply (f, t)) }

  | p=pauli
      { mk_expr $startpos $endpos (LambdaPC.Force p) }

  | LPAREN t=expr RPAREN
      { t }

pclif:
  | LAM x=ID COLON tp=ptype DOT body=expr
      { let xid = mk_ident x $startpos(x) $endpos(x) in
        mk_pc $startpos $endpos (LambdaPC.Lam { x = xid; tp; body }) }

  | LPAREN f=pclif RPAREN
      { f }

pauli:
  | SUSPEND t=expr
      { mk_p $startpos $endpos (LambdaPC.Suspend t) }
