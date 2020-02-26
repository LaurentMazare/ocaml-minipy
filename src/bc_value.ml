open Base
open Import

module Type_ = struct
  type t =
    | None_t
    | Bool
    | Int
    | Float
    | Tuple
    | Dict
    | List
    | Str
    | Builtin_fn
    | Function
    | Object
    | Class
    | Code
    | Iterator
  [@@deriving sexp]

  let to_string t = sexp_of_t t |> Sexp.to_string_mach
end

type z_t = Z.t

let sexp_of_z_t z = Z.to_string z |> sexp_of_string

type t =
  | None
  | Bool of bool
  | Int of z_t
  | Float of float
  | Tuple of t array
  | List of t array
  | Dict of (t, t) Hashtbl.Poly.t
  | Str of string
  | Builtin_fn of
      { name : string
      ; fn : t list -> t
      }
  | Function of
      { name : string
      ; code : t Bc_code.t
      ; args : Ast.arguments
      ; defaults : t list
      }
  | Code of
      { code : t Bc_code.t
      ; args : Ast.arguments
      }
  | Iterator of { next : unit -> t option }
[@@deriving sexp_of]

let type_ = function
  | None -> Type_.None_t
  | Bool _ -> Bool
  | Int _ -> Int
  | Float _ -> Float
  | Tuple _ -> Tuple
  | List _ -> List
  | Dict _ -> Dict
  | Str _ -> Str
  | Builtin_fn _ -> Builtin_fn
  | Function _ -> Function
  | Code _ -> Code
  | Iterator _ -> Iterator

let to_string ?(escape_special_chars = true) t =
  let rec loop ~e = function
    | None -> "None"
    | Bool true -> "True"
    | Bool false -> "False"
    | Int i -> Z.to_string i
    | Float f -> Float.to_string f
    | Tuple [| t |] -> "(" ^ loop ~e:true t ^ ",)"
    | Tuple ts ->
      Array.to_list ts
      |> List.map ~f:(loop ~e:true)
      |> String.concat ~sep:", "
      |> fun s -> "(" ^ s ^ ")"
    | List ts ->
      Array.to_list ts
      |> List.map ~f:(loop ~e:true)
      |> String.concat ~sep:", "
      |> fun s -> "[" ^ s ^ "]"
    | Dict dict ->
      Hashtbl.to_alist dict
      |> List.map ~f:(fun (key, value) -> loop ~e:true key ^ ": " ^ loop ~e:true value)
      |> String.concat ~sep:","
      |> fun s -> "{" ^ s ^ "}"
    | Str s -> if e then "\'" ^ String.escaped s ^ "\'" else s
    | Builtin_fn { name; fn = _ } -> Printf.sprintf "builtin<%s>" name
    | Function { name; _ } -> Printf.sprintf "function<%s>" name
    | Code _ -> "<code>"
    | Iterator _ -> "<iterator>"
  in
  loop t ~e:escape_special_chars

type code = t Bc_code.t [@@deriving sexp_of]

let str_exn = function
  | Str str -> str
  | t -> errorf "expected string, got '%s'" (type_ t |> Type_.to_string)

let code_exn = function
  | Code c -> c.code, c.args
  | t -> errorf "expected code, got '%s'" (type_ t |> Type_.to_string)

let none = None
let bool b = Bool b
let int i = Int i
let float f = Float f
let str s = Str s
let tuple ts = Tuple ts
let list ts = List ts
let code code ~args = Code { code; args }
let iterator ~next = Iterator { next }

let cannot_be_interpreted_as v str =
  errorf "%s cannot be interpreted as %s" (type_ v |> Type_.to_string) str

let to_bool t =
  match t with
  | None -> false
  | Bool b -> b
  | Int i -> not (Z.equal i Z.zero)
  | Float f -> Float.( <> ) f 0.
  | Tuple t | List t -> Array.length t > 0
  | Dict d -> Hashtbl.length d > 0
  | Str s -> String.length s > 0
  | t -> cannot_be_interpreted_as t "bool"

let to_float t =
  match t with
  | Bool true -> 1.
  | Bool false -> 0.
  | Float f -> f
  | Int i -> Z.to_float i
  | v -> cannot_be_interpreted_as v "float"

let to_int v =
  match v with
  | Bool true -> Z.one
  | Bool false -> Z.zero
  | Int i -> i
  | v -> cannot_be_interpreted_as v "int"

let array_iterator array =
  let current = ref 0 in
  let next () =
    if !current < Array.length array
    then (
      let v = array.(!current) in
      Int.incr current;
      Some v)
    else None
  in
  Iterator { next }

let to_iterable v =
  match v with
  | List l | Tuple l -> array_iterator l
  | Str s ->
    String.to_array s |> Array.map ~f:(fun c -> Str (Char.to_string c)) |> array_iterator
  | Dict s -> Hashtbl.keys s |> Array.of_list |> array_iterator
  | Iterator _ as it -> it
  | o -> cannot_be_interpreted_as o "iterable"
