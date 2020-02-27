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

module String_map = Map.M (String)

let check_empty_kwargs kwargs =
  if not (Map.is_empty kwargs)
  then
    errorf
      "no keyword argument expected, got %s"
      (Map.keys kwargs |> String.concat ~sep:",")

let empty_kwargs = Map.empty (module String)
