(* top-level interface for interacting with LambdaPC via the interpreter or other *)

include LambdaPC.HOAS
module S2 = Scalars.Scalars (Scalars.FIN2)
module Eval2 = LambdaPC.Eval(S2)
module Typing2 = Typing.SmtLambdaPC(S2)

let parse (s : string) : LambdaPC.Expr.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
let pc (s : string) : LambdaPC.Expr.pc =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.pcprog Lexer.read lexbuf in
  ast

let parseFromFile (filename : string) : LambdaPC.Expr.t =
  let f = In_channel.open_bin filename in
  let lexbuf = Lexing.from_channel f in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let eval e =
  print_endline (LambdaPC.Expr.pretty_string_of_t e ^ "\n->*\n");
  let result = Eval2.evalClosed e in
  print_endline (LambdaPC.Val.string_of_t result ^ "\n")

let typecheck pc =
  let (tp1,tp2) = Typing2.typecheck pc in
  print_endline @@ LambdaPC.Expr.pretty_string_of_pc pc ^ " : |" ^ LambdaPC.Type.string_of_t tp1 ^ " -o " ^ LambdaPC.Type.string_of_t tp2 ^"|"