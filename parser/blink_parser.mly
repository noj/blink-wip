%{
    (** 
     * The Blink parser
     *
     * TODO:
     *   - uint64 representation
     *   - sym_val UINT/INT conflict needs to be resolved
     *   - Annotations
     *)

    module A = Blink_ast

    (* Raise a syntax error with the current location *)
    let err l m =
      let loc_str = A.loc_to_string (A.loc_of_lexbuf l) in
        raise (Error.Syntax (Printf.sprintf "%s: Syntax error: %s" loc_str m))

    let trace l m =
      let loc_str = A.loc_to_string (A.loc_of_lexbuf l) in
        Printf.printf "%s: %s\n" loc_str m
%}

%token <Int64.t> HEXNUM
%token <Int64.t> UINT (* TODO: u64! *)
%token <Int64.t> INT

%token <string> CONTENT_TYPE
%token <string> NCNAME

%token AT
%token BAR
%token COLON
%token COMMA
%token DOT
%token EQ
%token QMARK
%token RARROW
%token NAMESPACE
%token SLASH
%token BACKSLASH
%token STAR
%token LBRACKET RBRACKET

%token I8 U8
%token I16 U16
%token I32 U32
%token I64 U64
%token F64 DECIMAL
%token DATE TIMEOFDAY
%token NANOTIME MILLITIME
%token BOOL STRING
%token OBJECT
%token TYPE SCHEMA

%token EOF

%start <Blink_ast.schema> schema

%%

schema:
    | d=defs EOF {
        { A.ns = None
        ; defs = d
        ; schema_loc = A.loc_of_lexbuf $startpos }
      }
    | NAMESPACE nsname=NCNAME d=defs EOF {
        { A.ns = Some nsname
        ; defs = d
        ; schema_loc = A.loc_of_lexbuf $startpos }
      }

defs: ds = list(def)    { ds }

def:
    | d=define      { d }
    | gd=group_def  { gd }

define:
    | n=name EQ e=enum          { A.Define (n, e, A.loc_of_lexbuf $startpos) }
    | n=name EQ t=field_type    { A.Define (n, A.TypeAlias t, A.loc_of_lexbuf $startpos) }
    | error EQ enum             { err $startpos "expected name before '='" }
    | name EQ error             { err $startpos "expected enum or field type after '='" }

group_def:
    | gn=group_name {
        let body = ([], A.loc_of_lexbuf $startpos) in
          A.GroupDef (gn, body, A.loc_of_lexbuf $startpos)
    }
    | gn=group_name RARROW fs=fields {
        let body = (fs, A.loc_of_lexbuf $startpos) in
          A.GroupDef (gn, body, A.loc_of_lexbuf $startpos)
    }

fields: fs=separated_nonempty_list(COMMA, field) { fs }

field:
    | t=field_type n=name           { A.Field (n, t, A.loc_of_lexbuf $startpos) }
    | t=field_type n=name QMARK     { A.OptionalField (n, t, A.loc_of_lexbuf $startpos) }

field_type:
    | t=single                      { A.Single t }
    | t=single LBRACKET RBRACKET    { A.Sequence t }

single:
    | I8                        { A.Number A.I8 }
    | U8                        { A.Number A.U8 }
    | I16                       { A.Number A.I16 }
    | U16                       { A.Number A.U16 }
    | I32                       { A.Number A.I32 }
    | U32                       { A.Number A.U32 }
    | I64                       { A.Number A.I64 }
    | U64                       { A.Number A.U64 }
    | DECIMAL                   { A.Decimal }
    | F64                       { A.F64 }
    | STRING                    { A.String None }
    | STRING ct=CONTENT_TYPE    { A.String (Some ct) }
    | DATE                      { A.Time A.Date }
    | TIMEOFDAY                 { A.Time A.TimeOfDay }
    | NANOTIME                  { A.Time A.NanoTime }
    | MILLITIME                 { A.Time A.MilliTime }
    | BOOL                      { A.Bool }
    | r=ref                     { r }
    | OBJECT                    { A.Object }
    | error                     { err $startpos "expected type" }

enum:
    | ss=separated_nonempty_list(BAR, sym)      { A.Enum ss }
    | BAR ss=separated_nonempty_list(BAR, sym)  { A.Enum ss }

group_name:
    | n=name i=option(preceded(SLASH, id)) p=option(preceded(COLON, name))
        { { A.group_name=n; group_id=i; parent=p } }

ref:
    | q=qname       { A.Ref (A.Regular, q) }
    | q=qname STAR  { A.Ref (A.Dynamic, q) }

sym: n=name v=option(preceded(SLASH, sym_val))  { {A.sym_name=n; sym_val=ref v} }

sym_val:
   | i=INT          { i }
   | u=UINT         { (* TODO: Should really by INT!*) u }
   | h=HEXNUM       { h }
   | error          { err $startpos "expected int or hexnum symbol value" }

id:
    | i=UINT        { i }
    | h=HEXNUM      { h }
    | error         { err $startpos "expected uint or hexnum id" }

qname:
    | n=NCNAME  { n }
    | n=cname   { n }

cname:
    | n1=NCNAME COLON n2=NCNAME { n1 ^ ":" ^ n2 }

name: n=NCNAME              { n }
    | BACKSLASH n=NCNAME    { n }
    (* TODO: This is lame: *)
    | BACKSLASH I8          { "i8" }
    | BACKSLASH U8          { "u8" }
    | BACKSLASH I16         { "i16" }
    | BACKSLASH U16         { "u16" }
    | BACKSLASH I32         { "i32" }
    | BACKSLASH U32         { "u32" }
    | BACKSLASH I64         { "i64" }
    | BACKSLASH U64         { "u64" }
    | BACKSLASH DECIMAL     { "decimal" }
    | BACKSLASH F64         { "f64" }
    | BACKSLASH STRING      { "string" }
    | BACKSLASH DATE        { "date" }
    | BACKSLASH TIMEOFDAY   { "timeOfDay" }
    | BACKSLASH NANOTIME    { "nanotime" }
    | BACKSLASH MILLITIME   { "millitime" }
    | BACKSLASH BOOL        { "bool" }
    | BACKSLASH OBJECT      { "object" }
    | BACKSLASH SCHEMA      { "schema" }
    | BACKSLASH TYPE        { "type" }

