(** Print a message to stderr *)
let err_print s =
  output_string stderr s ; output_string stderr "\n"; flush stderr

(** Execute function 'fn', and ensure that the 'finally' function always gets
  * run, even though the function throws an exception.
  * Typical usage is for guarding resources, such as file handles
  *)
let protect fn finally =
  try
    let res = fn () in
      finally ();
      res
  with _ as exn -> finally (); raise exn

(** Run function 'fn' on the list 'l', unless it's empty, then an 'msg' will be
  * output on stderr.
  *)
let iter_list fn l msg =
  match l with
      []    -> err_print msg
    | _     -> List.iter fn l

let option_default o def =
  match o with
      None -> def
    | Some s -> s

