%{
open LambdaPC
%}

%token <int> ID
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



%start <LambdaPC.Expr.t> prog
%start <LambdaPC.Expr.pc> pcprog

%type <LambdaPC.Expr.t> expr
%type <LambdaC.Expr.t> lexpr
%type <LambdaPC.Expr.pc> pclif
%type <LambdaPC.Expr.p> pauli

%type <LambdaPC.Type.t> ptype
%type <LambdaC.Type.t> ltype

%%

(* RULES *)
prog:
(*   | LET x=ID EQUAL f=pclif SEMICOLON prog
      {  } *)
    | e=expr EOF {e}

pcprog:
    | f=pclif EOF {f}

ptype:
    | PAULI { LambdaPC.Type.Pauli }
    | tp1=ptype TENSOR tp2=ptype
        { LambdaPC.Type.PTensor (tp1,tp2) }
    | LPAREN tp=ptype RPAREN
      { tp }

ltype:
    | UNIT
      { LambdaC.Type.Unit }
    | tp1=ltype PLUS tp2=ltype
      { LambdaC.Type.Sum (tp1, tp2) }
    | tp1=ltype LOLLI tp2=ltype
      { LambdaC.Type.Arrow (tp1, tp2) }
    | LPAREN tp=ltype RPAREN
      { tp }

lexpr:
  | LVARTAG x=ID { LambdaC.Expr.Var x }
  | ZERO LCURLY tp=ltype RCURLY
    { LambdaC.Expr.Zero tp }
  | XCONST { LambdaC.Expr.Pair (LambdaC.Expr.Const 1, LambdaC.Expr.Const 0) }
  | YCONST { LambdaC.Expr.Pair (LambdaC.Expr.Const 1, LambdaC.Expr.Const 1) }
  | ZCONST { LambdaC.Expr.Pair (LambdaC.Expr.Const 0, LambdaC.Expr.Const 1) }
  | ICONST { LambdaC.Expr.Pair (LambdaC.Expr.Const 0, LambdaC.Expr.Const 0) }

  | a1=lexpr PLUS a2=lexpr
    { LambdaC.Expr.Plus (a1, a2) }
  | r=INT
    { LambdaC.Expr.Const r }
  | a1=lexpr DOT STAR a2=lexpr
    { LambdaC.Expr.Scale (a1, a2) }
  | LSQUARE a1=lexpr COMMA a2=lexpr RSQUARE
    { LambdaC.Expr.Pair (a1, a2) }
  | DOT CASE a=lexpr OF LCURLY IN1 x1=ID ARROW a1=lexpr
                    MID    IN2 x2=ID ARROW a2=lexpr RCURLY
    { LambdaC.Expr.Case (a, x1, a1, x2, a2) }
  | LAM x=ID COLON tp=ltype DOT a=lexpr
    { LambdaC.Expr.Lambda (x, tp, a) }
  | a1=lexpr DOT AT a2=lexpr
    { LambdaC.Expr.Apply (a1, a2) }

  | LPAREN a=lexpr RPAREN
    { a }

expr:
  | x=ID
    { Expr.Var x }
  | LET x=ID EQUALS t1=expr IN t2=expr
    { Expr.Let (t1, x, t2) }
  | a=lexpr
    { Expr.LExpr a }
  | LANGLE a=lexpr RANGLE t=expr
    { Expr.Phase (a, t) }
  | t1=expr STAR t2=expr
    { Expr.Prod (t1, t2) }
  
  | t=expr CARROT n=INT
    { Expr.Pow (t, LambdaC.Expr.Const n) }
  | t=expr CARROT LCURLY n=INT RCURLY
    { Expr.Pow (t, LambdaC.Expr.Const n) }
  | t=expr CARROT LCURLY a=lexpr RCURLY
    { Expr.Pow (t, a) }

  | CASE t=expr OF LCURLY XCONST ARROW tx=expr MID ZCONST ARROW tz=expr RCURLY
    { Expr.CasePauli (t, tx, tz) }
  | CASE t=expr OF LCURLY ZCONST ARROW tz=expr MID XCONST ARROW tx=expr RCURLY
    { Expr.CasePauli (t, tx, tz) }

  | IN1 LCURLY tp=ptype RCURLY t=expr
    { Expr.In1 (t, tp) }
  | IN2 LCURLY tp=ptype RCURLY t=expr
    { Expr.In2 (tp, t) }
  | CASE t=expr OF LCURLY IN1 x1=ID ARROW t1=expr
                   MID    IN2 x2=ID ARROW t2=expr RCURLY
    { Expr.CasePTensor (t, x1, t1, x2, t2) }
  | CASE t=expr OF LCURLY IN2 x2=ID ARROW t2=expr
                   MID    IN1 x1=ID ARROW t1=expr RCURLY
    { Expr.CasePTensor (t, x1, t1, x2, t2) }

  | f=pclif AT t=expr
    { Expr.Apply(f, t) }
  | p=pauli
    { Expr.Force p }

  | LPAREN t=expr RPAREN
    { t }

pclif:
  | LAM x=ID COLON tp=ptype DOT t=expr
    { Expr.Lam (x, tp, t) }
  | LPAREN f=pclif RPAREN
    { f }

pauli:
  | SUSPEND t=expr
    { Expr.Suspend t }
