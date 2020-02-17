open Base

exception RuntimeError of string

let errorf fmt = Printf.ksprintf (fun s -> raise (RuntimeError s)) fmt

module type Interp = sig
  type env
  type value

  val has_method : env -> value -> string -> bool
  val call_method : env -> value -> string -> value list -> value
end

let empty_attrs () = Hashtbl.create (module String)
