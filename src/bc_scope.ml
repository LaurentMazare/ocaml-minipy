open Base

type t = (string, Bc_value.t) Hashtbl.t

let create () = Hashtbl.create (module String)
let find t key = Hashtbl.find t key
let mem t key = Hashtbl.mem t key
let set t key data = Hashtbl.set t ~key ~data
let remove t key = Hashtbl.remove t key
let of_alist_exn = Hashtbl.of_alist_exn (module String)
