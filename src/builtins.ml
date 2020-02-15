open! Base

let default : Value.builtins =
  let print args _kwargs =
    List.map args ~f:(Value.to_string ~escape_special_chars:false)
    |> String.concat ~sep:" "
    |> Stdio.printf "%s\n";
    Value.none
  in
  let to_int v = Value.to_int v |> Z.to_int in
  let of_int i = Z.of_int i |> Value.int in
  let range args _kwargs =
    let l =
      match args with
      | [ v ] -> List.range 0 (to_int v)
      | [ v1; v2 ] -> List.range (to_int v1) (to_int v2)
      | [ v1; v2; s ] -> List.range (to_int v1) (to_int v2) ~stride:(to_int s)
      | _ -> failwith "range expects one, two, or three arguments"
    in
    Value.list (List.map l ~f:of_int |> Queue.of_list)
  in
  let len args _kwargs =
    let l =
      match (args : Value.t list) with
      | [ Val_tuple l ] -> Array.length l
      | [ Val_list l ] -> Queue.length l
      | [ Val_str s ] -> String.length s
      | [ Val_dict d ] -> Hashtbl.length d
      | [ v ] -> Value.cannot_be_interpreted_as v "type with len"
      | _ -> failwith "len takes exactly one argument"
    in
    of_int l
  in
  Map.of_alist_exn (module String) [ "print", print; "range", range; "len", len ]
