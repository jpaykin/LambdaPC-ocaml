{
    open Parser

    exception LexError of string * Lexing.position * Lexing.position
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | digit | '_' | '\'')*

rule read =
  parse
  | white { read lexbuf }
  | '\n'
    {
      Lexing.new_line lexbuf;
      read lexbuf
    }
  | "var" {LVARTAG}
  | "let" { LET }
  | "in" { IN }
  | "=" { EQUALS }
  | "case" { CASE }
  | "of" { OF }
  | "->" { ARROW }
  | "|" {MID}
  | "lambda" {LAM}
  | ":" {COLON}
  | "." {DOT}
  | "suspend" {SUSPEND}
  | "," {COMMA}

  | "X" {XCONST}
  | "Y" {YCONST}
  | "Z" {ZCONST}
  | "I" {ICONST}
  | "in1" {IN1}
  | "in2" {IN2}
  | "zero" {ZERO}

  | "*" {STAR}
  | "^" {CARROT}
  | "@" {AT}

  | "Pauli" {PAULI}
  | "Zd" {UNIT}
  | "+" {PLUS}
  | "-o" {LOLLI}
  | "**" {TENSOR}

  | "<" {LANGLE}
  | ">" {RANGLE}
  | "{" {LCURLY}
  | "}" {RCURLY}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" {LSQUARE}
  | "]" {RSQUARE}

  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (String.hash (Lexing.lexeme lexbuf)) }

  | eof { EOF }

  | _ as c
      {
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        raise (LexError (Printf.sprintf "Unexpected character: %C" c, sp, ep))
      }
