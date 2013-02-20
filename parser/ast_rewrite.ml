(** This represents the first pass over the AST *)
open Blink_ast

module StringSet = Set.Make(struct
                              type t = string
                              let compare = String.compare
                            end)
module Int64Set = Set.Make(struct
                             type t = Int64.t
                             let compare = In64.compare
                           end)

type env = {
  env_ns: string ref (* Current namespace *)
  env_defs: StringSet ref
}

let env_is_def_uniq env def =
  StringSet.mem def !(env.env_defs)

let env_add_def env def =
  env.env_defs := StringSet.add def !(env.env_defs)


type qname = {
  qn_ns: string option;
  qn_name: string
}

let qname_to_string q =
  match q.qn_ns with
      None -> q.qn_name
    | Some ns -> ns ^ ":" ^ q.qn_name

(** Names:
  *   - All definition names must be unique within a namespace.
  *   - Field names must be unique within a group and must not shadow any inherited fields.
  *)

(** Types:
  *   - A sequence type *must not* reference another sequence type, directly or indirectly.
  *   - A type reference used in inheritance must resolve to a group type.
  *)

let incref r =
  r := Int64.add (!r) (Int64.of_int 1)


let ensure_unique_sym sym set errmsg =
  if StringSet.mem sym set then failwith errmsg else StringSet.add sym set

let ensure_unique_id id set errmsg =
  if Int64Set.mem sym set then failwith errmsg else Int64Set.add sym set

(* Adds enum values to enum members which are implicitly defined. *)
let fix_enums es =
  (* Default implicit enum value starts at 0 *)
  let implicit = ref (Int64.of_int 0) in

  let seen_names = ref StringSet.empty in
  let seen_values = ref Int64Set.empty in

    (* Add enum values for enum members with missing values: *)
    List.iter (fun s -> match !(s.sym_val) with
                   None -> s.sym_val := Some (!implicit); incref implicit
                 | Some v -> implicit:= v; incref implicit) es;

    (* Validate enum:
     *   - Symbols must be unique within enum, values must be distinct.
     *)
    List.iter (fun s -> 
                   None -> failwith "BUG: Enum with None!"
                 | Some v -> seen_names := ensure_unique_sym s.sym_name seen_names (sym_name ^ " not unique in enum");
                             seen_values := ensure_unique_id s.sum_val seen_values "enum value used multiple times"
    ) es


let rewrite_def_type env = function
  | Enum syms -> fix_enums syms
  | TypeAlias _ -> () (* TODO *)

let rewrite_def env = function
  | Define (defname, deft, loc) -> rewrite_def_type env deft
  | GroupDef (_, _, loc) -> ()  (* TODO *)

let rewrite ast =
  let env = {env_ns = ref (Util.option_default ast.ns "")} in
    List.iter (rewrite_def env) ast.defs

