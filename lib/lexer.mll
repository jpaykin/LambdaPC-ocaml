{
    open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
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

  | id { ID (String.hash (Lexing.lexeme lexbuf)) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }