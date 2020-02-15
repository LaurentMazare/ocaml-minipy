open Base

exception RuntimeError of string

let errorf fmt = Printf.ksprintf (fun s -> raise (RuntimeError s)) fmt
