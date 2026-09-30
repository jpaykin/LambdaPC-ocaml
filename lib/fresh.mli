(** Unified fresh-id API

    Example:
    {[
      Fresh.seed 41;
      let x = Fresh.fresh ~hint:"tmp" () in
      assert (x > 41)
    ]}

    From resolved surface AST:

    {[
      let expr = Interface.parse "let x = X in x" in
      Named_ast.LambdaPC_Surface.seed_fresh expr;
      let y = Fresh.fresh ~hint:"eval" ()
    ]}

    From a core AST:

    {[
      let env = LambdaC.VariableEnvironment.create () in
      LambdaC.Expr.update_env env expr;
      let z = Fresh.fresh ~hint:"normalize" ()
    ]}
*)

type id = int

val current : unit -> id
val seed : id -> unit
val seed_many : id list -> unit
val fresh : ?hint:string -> unit -> id
