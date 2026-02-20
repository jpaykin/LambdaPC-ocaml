(* top-level interface for interacting with LambdaPC via the interpreter or other *)
include LambdaPC.HOAS

module S2 = Scalars.Scalars (Scalars.FIN2)
module LEval2 = LambdaC.Eval(S2.Zd)
module Eval2 = LambdaPC.Eval(S2)
module Typing2 = Typing.SmtLambdaPC(S2)

module S3 = Scalars.Scalars (Scalars.FIN3)
module LEval3 = LambdaC.Eval(S3.Zd)
module Eval3 = LambdaPC.Eval(S3)
module Typing3 = Typing.SmtLambdaPC(S3)

module S4 = Scalars.Scalars (Scalars.FIN4)
module LEval4 = LambdaC.Eval(S4.Zd)
module Eval4 = LambdaPC.Eval(S4)
module Typing4 = Typing.SmtLambdaPC(S4)

let dim = ref 2
let set_dimension d = dim := d

exception Parse_error of string * string

let set_file (lb : Lexing.lexbuf) (file : string) : unit =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- { p with pos_fname = file }

let parse_with
  (file : string)
  (lb : Lexing.lexbuf)
  (entry : Lexing.lexbuf -> 'a)
  : 'a
=
  set_file lb file;
  try entry lb with
  | Lexer.LexError (msg, sp, ep) ->
      raise (Parse_error (Util.loc_string_of_positions sp ep, "Lexing error: " ^ msg))
  | Parser.Error ->
      let sp, ep = Util.get_start_end lb in
      let near =
        let lx = Lexing.lexeme lb in
        if lx = "" then "end of input" else Printf.sprintf "%S" lx
      in
      raise (Parse_error (Util.loc_string_of_positions sp ep, "Parse error near " ^ near))

let parse (s : string) : Named_ast.LambdaPC.expr =
  let lexbuf = Lexing.from_string s in
  parse_with "<stdin>" lexbuf (fun lb -> Parser.prog Lexer.read lb)
  (*let ast = Parser.prog Lexer.read lexbuf in
  ast*)

let pc (s : string) : Named_ast.LambdaPC.pc =
  let lexbuf = Lexing.from_string s in
  parse_with "<stdin>" lexbuf (fun lb -> Parser.pcprog Lexer.read lb)
  (* let ast = Parser.pcprog Lexer.read lexbuf in *)
  (* ast *)

let parseFromFile (filename : string) : Named_ast.LambdaPC.expr =
  let f = In_channel.open_bin filename in
  let lexbuf = Lexing.from_channel f in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let eval e =
  print_endline (LambdaPC.Expr.pretty_string_of_t e ^ "\n->*\n");
  let eval_closed = match !dim with
             | 2 -> Eval2.evalClosed
             | 3 -> Eval3.evalClosed
             | 4 -> Eval4.evalClosed
             | d -> failwith @@ "Please add evaluation module for dimension " ^ string_of_int d ^ "\n"
  in
  let result = eval_closed e in
  print_endline (LambdaPC.Val.string_of_t result ^ "\n")


let leval e =
  print_endline (LambdaC.Expr.pretty_string_of_t e ^ "\n->*\n");
  let eval_closed = match !dim with
             | 2 -> LEval2.eval LambdaC.VariableMap.empty
             | 3 -> LEval3.eval LambdaC.VariableMap.empty
             | 4 -> LEval4.eval LambdaC.VariableMap.empty
             | d -> failwith @@ "Please add evaluation module for dimension " ^ string_of_int d ^ "\n"
  in
  let result = eval_closed e in
  print_endline (LambdaC.Val.string_of_t result ^ "\n")


let omega tp e1 e2 =
  leval LambdaPC.SymplecticForm.(omega tp (psi_of e1) (psi_of e2))

let typecheck pc =
  let typecheck_d = match !dim with
             | 2 -> Typing2.typecheck
             | 3 -> Typing3.typecheck
             | 4 -> Typing4.typecheck
             | d -> failwith @@ "Please add typing module to interface.ml for dimension " ^ string_of_int d ^ "\n"
  in
  let (tp1,tp2) = typecheck_d pc in
  print_endline @@ LambdaPC.Expr.pretty_string_of_pc pc ^ " : |" ^ LambdaPC.Type.string_of_t tp1 ^ " -o " ^ LambdaPC.Type.string_of_t tp2 ^"|"
