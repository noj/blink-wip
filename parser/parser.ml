(** Parser frontend for Blink schemas *)
open Blink_ast
open Blink_parser
open Util

let usage = "Usage: parser [-p] file [file ...]"

let parse_debug = ref false

let parse_files = ref []

let options = Arg.align [
  ("-p", Arg.Set parse_debug, " print parsing debug strings")
]

let add_file filename = (parse_files := filename :: !parse_files)

let parse_file fname =
  let fh = open_in fname in
    protect
      (fun () ->
         let lexbuf = Lexing.from_channel fh in
           lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname};
           Blink_parser.schema Blink_lexer.token lexbuf)
      (fun () ->
         ignore (close_in fh))

let _ =
  Arg.parse options add_file usage ;
  Util.iter_list (fun fn ->
                    try
                      let tree = parse_file (List.nth !parse_files 0) in
                        Blink_ast.pp tree
                    with Error.Syntax msg -> Printf.eprintf "%s\n" msg) !parse_files usage
