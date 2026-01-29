
let get_start_end (lb : Lexing.lexbuf) : Lexing.position * Lexing.position =
  Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb

let string_of_token : Parser.token -> string = function
  | Parser.CASE -> "CASE"
  | Parser.OF -> "OF"
  | Parser.LCURLY -> "LCURLY"
  | Parser.RCURLY -> "RCURLY"
  | Parser.IN1 -> "IN1"
  | Parser.IN2 -> "IN2"
  | Parser.ARROW -> "ARROW"
  | Parser.MID -> "MID"
  | Parser.ID _ -> "ID"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.DOT -> "DOT"
  | Parser.COLON -> "COLON"
  | Parser.PAULI -> "PAULI"
  | Parser.TENSOR -> "TENSOR"
  | Parser.LAM -> "LAM"
  | Parser.EOF -> "EOF"
  | _ -> "OTHER"

let dump_tokens ~(file : string) (s : string) : unit =
  let lb = Lexing.from_string s in
  let p = lb.Lexing.lex_curr_p in
  lb.Lexing.lex_curr_p <- { p with Lexing.pos_fname = file };
  let rec loop () =
    let sp, ep = get_start_end lb in
    let tok = Lexer.read lb in
    Printf.printf "%s  lexeme=%S  @ %d:%d-%d:%d\n"
      (string_of_token tok) (Lexing.lexeme lb)
      sp.pos_lnum (sp.pos_cnum - sp.pos_bol)
      ep.pos_lnum (ep.pos_cnum - ep.pos_bol);
    match tok with
    | Parser.EOF -> ()
    | _ -> loop ()
  in
  loop ()

let col (p : Lexing.position) : int = p.pos_cnum - p.pos_bol

let loc_string_of_positions (sp : Lexing.position) (ep : Lexing.position) : string =
  let file = sp.Lexing.pos_fname in
  let sl, sc = sp.Lexing.pos_lnum, col sp in
  let el, ec = ep.Lexing.pos_lnum, col ep in
  if sl = el then Printf.sprintf "%s:%d:%d-%d" file sl sc ec
  else Printf.sprintf "%s:%d:%d-%d:%d" file sl sc el ec
