{
    open Blink_parser
    open Printf
}

let digit = ['0'-'9']
let _uint = digit+
let _int = '-'? _uint

let nameStartChar = ['_' 'a'-'z' 'A'-'Z']
let nameChar = (nameStartChar | digit)
let ncName = nameStartChar nameChar*

let hexDigits = ['0'-'9' 'a'-'f' 'A'-'F']+
let hexNum = "0x" hexDigits

let ctChar = [^')''\n']
let contentType = '(' ctChar+ ')'

rule token = parse
    | '\n'              { Lexing.new_line lexbuf; token lexbuf }
    | [' ' '\r' '\t']   { token lexbuf }

    (* Keywords: *)
    | "namespace"       { NAMESPACE }
    | "i8"              { I8 }
    | "u8"              { U8 }
    | "i16"             { I16 }
    | "u16"             { U16 }
    | "i32"             { I32 }
    | "u32"             { U32 }
    | "i64"             { I64 }
    | "u64"             { U64 }
    | "f64"             { F64 }
    | "decimal"         { DECIMAL }
    | "date"            { DATE }
    | "timeOfDay"       { TIMEOFDAY }
    | "nanotime"        { NANOTIME }
    | "millitime"       { MILLITIME }
    | "bool"            { BOOL }
    | "string"          { STRING }
    | "object"          { OBJECT }
    | "type"            { TYPE }
    | "schema"          { SCHEMA }

    | ncName            { NCNAME (Lexing.lexeme lexbuf) }
    | _uint             { UINT (Int64.of_string (Lexing.lexeme lexbuf)) }
    | _int              { INT (Int64.of_string (Lexing.lexeme lexbuf)) }
    | hexNum            { HEXNUM (Int64.of_string (Lexing.lexeme lexbuf)) }
    | contentType       { let s = Lexing.lexeme lexbuf in
                          let len = String.length s in
                            CONTENT_TYPE (String.sub s 1 (len-2)) }

    | '@'               { AT }
    | '|'               { BAR }
    | ':'               { COLON }
    | ','               { COMMA }
    | '.'               { DOT }
    | '='               { EQ }
    | '?'               { QMARK }
    | "->"              { RARROW }
    | '/'               { SLASH }
    | '\\'              { BACKSLASH }
    | '*'               { STAR }
    | '['               { LBRACKET }
    | ']'               { RBRACKET }

    | '#'               { line_comment lexbuf }

    | eof               { EOF }

(* Parses to end-of-line *)
and line_comment = parse
    | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
    | _             { line_comment lexbuf }
{
}
