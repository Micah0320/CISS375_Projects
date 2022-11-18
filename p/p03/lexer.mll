(* file: lexer1.mll *)
(* Lexical analyzer returns one of the tokens: *)
{
open Parser (* Assumes the parser file is "parser.mly". *);;
open Lexing
let inc_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    }
}

let digits = ['0'-'9']
let assignment = ":="
let delim = [';']
let int = digits+
let float = digits* '.' digits+
let numError = int '.'
let num = int | float
let program = "PROGRAM"
let begin = "BEGIN"
let end = "END"
let intType = "INTEGER"
let realType = "REAL"
let stringType = "STRING"
let string = '"'_*'"'
let letters = ['a' - 'z']
let idDig = (letters|digits)
let id = letters (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)?
let space = ' ' | '\n'
let idError = letters (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)? (idDig)+
let write = "WRITE"
let commentSyntaxError = "ERROR: Invalid Comment Syntax"
let unendingCommentError = "ERROR: Unending Comment"

let commentError = commentSyntaxError | unendingCommentError
let pow = "**"



rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' {  inc_lineno lexbuf; NEWLINE }
| int as num {NUM (num ^ "I")}
| "." digits+ as num {NUM (num ^ "R")}
| digits+ "." digits* as num { NUM (num ^ "R") }
| string as s {STRING_VAL (s)}
| assignment {AOP}
| pow {CARET}
| '+' { PLUS }
| '-' { MINUS }
| '*' { MULTIPLY }
| '/' { DIVIDE }
| 'n' { MINUS }
| '(' { LPAREN }
| ')' { RPAREN }
| write { WRITE }
| program {PROGRAM}
| begin {BEGIN}
| end {END}
| intType {INTEGER}
| realType {REAL}
| stringType {STRING}
| id as name {Id (name)}
| delim {DELIM}
| _ { token lexbuf }
| eof { EOF }
