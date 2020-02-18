open Base

type t =
  | None
  | Bool of bool
  | Int of Z.t
  | Float of float
  | Tuple of t array
  | List of t array
  | Dict of (t, t) Hashtbl.Poly.t
  | Str of string

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
  in
  loop t ~e:escape_special_chars
