(** The Blink AST *)

open Printf

type loc = { 
  filename: string;
  line: int
}

type annot = {
  annot_key: string;
  annot_value: string;
  annot_loc: loc
}

let loc_of_lexbuf l =
  { filename = l.Lexing.pos_fname; line = l.Lexing.pos_lnum; }

let loc_to_string l =
  sprintf "%s:%d" l.filename l.line

type schema = {
  ns: string option;
  defs: def list;
  schema_loc: loc
}

and group = {
  group_name: string;
  group_id: Int64.t option;
  parent: string option
}

and def =
    Define of (string * def_type * loc)
  | GroupDef of (group * body * loc)

and def_type =
  | Enum of sym list
  | TypeAlias of field_type

and body = (field list * loc)

(* Fields *)
and field =
    Field of (string * field_type * loc)
  | OptionalField of (string * field_type * loc)

and field_type =
    Single of single
  | Sequence of single

and single =
    Number of number_type
  | String of string option (* Optional content-type *)
  | Time of time_type
  | Ref of (ref_type * string)
  | Bool
  | Object
  | Decimal
  | F64

and ref_type =
  | Regular
  | Dynamic

and number_type =
    I8 | U8
  | I16 | U16
  | I32 | U32
  | I64 | U64

and time_type =
    Date
  | TimeOfDay
  | NanoTime
  | MilliTime

and sym = {
  sym_name: string;
  sym_val: Int64.t option ref
}

(* Pretty printing the AST.
 * TODO: For some reason the indenting doesn't work correctly... *)
let number_type_to_string = function
  | I8 -> "i8"
  | U8 -> "u8"
  | I16 -> "i16"
  | U16 -> "u16"
  | I32 -> "i32"
  | U32 -> "u32"
  | I64 -> "i64"
  | U64 -> "u64"

let time_type_to_string = function
  | Date -> "date"
  | TimeOfDay -> "timeOfDay"
  | NanoTime -> "nanoTime"
  | MilliTime -> "milliTime"

let sym_to_string ppf s =
  match !(s.sym_val) with
    | None -> Format.fprintf ppf "| %s@," s.sym_name
    | Some v -> Format.fprintf ppf "| %s/%Ld@," s.sym_name v

let pp_single ppf = function
  | Number n -> Format.fprintf ppf "%s" (number_type_to_string n)
  | String None -> Format.fprintf ppf "string"
  | String (Some c) -> Format.fprintf ppf "string(%s)" c
  | Time t -> Format.fprintf ppf "%s" (time_type_to_string t)
  | Ref (_, r) -> Format.fprintf ppf "%s" r
  | Bool -> Format.fprintf ppf "bool"
  | Object -> Format.fprintf ppf "object"
  | Decimal -> Format.fprintf ppf "decimal"
  | F64 -> Format.fprintf ppf "f64"

let field_type_to_string ppf = function
  | Single f -> Format.fprintf ppf "@[<4>%a@]" pp_single f
  | Sequence f -> Format.fprintf ppf "@[<4>%a[]@]" pp_single f

let optional_id = function
  | None -> ""
  | Some i -> " / " ^ (Int64.to_string i)

let optional_str pfx = function
  | None -> ""
  | Some s -> pfx ^ s

let group_to_string ppf g =
  Format.fprintf ppf "%s%s%s" g.group_name (optional_id g.group_id) (optional_str " : " g.parent)

let field_to_string ppf = function
  | Field (n, t, _) -> Format.fprintf ppf "@[<v 3>%a %s@]@." field_type_to_string t n
  | OptionalField (n, t, _) -> Format.fprintf ppf "@[<v 3>%a %s?@]@." field_type_to_string t n

let body_to_string ppf b =
  let (bs, _) = b in
  match bs with
      [] -> Format.fprintf ppf "@]@."
    | bs' -> Format.fprintf ppf "->@]@."; List.iter (field_to_string ppf) bs'

let pp_def_type ppf = function
  | Enum s -> List.iter (sym_to_string ppf) s
  | TypeAlias ft -> Format.fprintf ppf "%a" field_type_to_string ft

let def_to_string ppf = function
  | Define (n, ft, _)   -> Format.fprintf ppf "@[<v 2>%s = %a@]@." n pp_def_type ft
  | GroupDef (g, b, _)  -> Format.fprintf ppf "@[<v 2>%a %a@]@." group_to_string g body_to_string b

let pprint ppf s =
  Format.fprintf ppf "@[<v 1>Schema (\"%s\" %s@." s.schema_loc.filename (optional_str "ns=" s.ns);
  List.iter (def_to_string ppf) s.defs;
  Format.fprintf ppf ")@]@."

let pp = pprint Format.std_formatter
