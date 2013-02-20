(* Lexer frontend for Blink schema definitions *)
open Blink_ast
open Blink_parser
open Util
open Printf

let usage = "Usage: lexer [-l] file [file ...]"

let lex_debug = ref true

let lex_files = ref []

let options = Arg.align [
  ("-l", Arg.Set lex_debug, " print lex debug strings")
]

let add_file filename = (lex_files := filename :: !lex_files)

let token_to_string = function
  | HEXNUM n -> sprintf "HEXNUM(0x%08Lx)" n
  | UINT n -> sprintf "UINT(%Ld)" n
  | INT n -> sprintf "INT(%Ld)" n
  | CONTENT_TYPE c -> sprintf "CONTENT_TYPE(%s)" c
  | NCNAME n -> sprintf "NCNAME(%s)" n
  | I8 -> "I8"
  | U8 -> "U8"
  | I16 -> "I16"
  | U16 -> "U16"
  | I32 -> "I32"
  | U32 -> "U32"
  | I64 -> "I64"
  | U64 -> "U64"
  | F64 -> "F64"
  | DECIMAL -> "DECIMAL"
  | DATE -> "DATE"
  | TIMEOFDAY -> "TIMEOFDAY"
  | NANOTIME -> "NANOTIME"
  | MILLITIME -> "MILLITIME"
  | BOOL -> "BOOL"
  | STRING -> "STRING"
  | OBJECT -> "OBJECT"
  | TYPE -> "TYPE"
  | SCHEMA -> "SCHEMA"
  | AT -> "AT"
  | BAR -> "BAR"
  | COLON -> "COLON"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | EQ -> "EQ"
  | QMARK -> "QMARK"
  | NAMESPACE -> "NAMESPACE"
  | RARROW -> "RARROW"
  | SLASH -> "SLASH"
  | BACKSLASH -> "BACKSLASH"
  | STAR -> "STAR"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | EOF -> "EOF"

let run_lex buf =
  let rec go buf =
    let tok = Blink_lexer.token buf in
    let pos = buf.Lexing.lex_curr_p in
    let line = pos.Lexing.pos_lnum in
    let file = pos.Lexing.pos_fname in
      match tok with
          EOF   -> ()
        | _     -> (if !lex_debug then Format.printf "%s:%d| %s\n" file line (token_to_string tok) else ()); go buf
  in
    go buf

let lex_file fname =
  try
    let fh = open_in fname in
      protect
        (fun () ->
           let lexbuf = Lexing.from_channel fh in
             lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname};
             run_lex lexbuf;
             if not !lex_debug then Format.printf "Ok\n" else ())
        (fun () ->
           ignore (close_in fh))
  with Sys_error msg -> err_print msg
    (*| SyntaxError _ as e -> Util.err_print (string_of_syntax_error e)*)

let _ =
  Arg.parse options add_file usage ;
  Util.iter_list lex_file !lex_files usage

