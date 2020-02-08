(* This has been adapted from:
   https://github.com/ocsigen/js_of_ocaml/blob/master/toplevel/examples/lwt_toplevel/toplevel.ml
*)
open Base
open Js_of_ocaml
module I = Minipy.Interpreter
module Parse = Minipy.Parse
module Format = Caml.Format
open Js_of_ocaml_tyxml

module Toploop : sig
  type t

  val create : unit -> t
  val execute : t -> pp_code:Format.formatter -> pp:Format.formatter -> string -> unit
end = struct
  type t = I.Env.t

  let create () = I.Env.empty ~builtins:I.default_builtins

  let protect ~pp ~f =
    try f () with
    | I.RuntimeError message -> Format.fprintf pp "RuntimeError: %s\n%!" message

  let eval_stmts t ~pp stmts = protect ~pp ~f:(fun () -> I.eval_stmts t stmts)

  let execute t ~pp_code ~pp content =
    Format.fprintf pp_code "%s\n%!" content;
    let stmts = Parse.parse_string (content ^ "\n") in
    match stmts with
    | Error { message; context } ->
      Format.fprintf pp "ParseError: %s\n%!" message;
      Option.iter context ~f:(fun c -> Format.fprintf pp "%s\n%!" c)
    | Ok stmts ->
      (match List.last stmts with
      | None -> ()
      | Some (Expr { value }) ->
        let stmts = List.drop_last_exn stmts in
        eval_stmts t ~pp stmts;
        protect ~pp ~f:(fun () ->
            let value = I.eval_expr t value in
            match value with
            | Val_none -> ()
            | value ->
              I.Value.to_string value ~escape_special_chars:false
              |> Format.fprintf pp "%s\n%!")
      | Some _ -> eval_stmts t ~pp stmts)
end

let by_id s = Dom_html.getElementById s

let by_id_coerce s f =
  Js.Opt.get (f (Dom_html.getElementById s)) (fun () -> raise Caml.Not_found)

module History = struct
  let data = ref [| "" |]
  let idx = ref 0

  let get_storage () =
    match Js.Optdef.to_option Dom_html.window##.localStorage with
    | exception _ -> raise Caml.Not_found
    | None -> raise Caml.Not_found
    | Some t -> t

  let setup () =
    try
      let s = get_storage () in
      match Js.Opt.to_option (s##getItem (Js.string "history")) with
      | None -> raise Caml.Not_found
      | Some s ->
        let a = Json.unsafe_input s in
        data := a;
        idx := Array.length a - 1
    with
    | _ -> ()

  let push text =
    let l = Array.length !data in
    let n = Array.create ~len:(l + 1) "" in
    !data.(l - 1) <- text;
    Array.blit ~src:!data ~src_pos:0 ~dst:n ~dst_pos:0 ~len:l;
    data := n;
    idx := l;
    try
      let s = get_storage () in
      let str = Json.output !data in
      s##setItem (Js.string "history") str
    with
    | Caml.Not_found -> ()

  let current text = !data.(!idx) <- text

  let previous textbox =
    if !idx > 0
    then (
      Int.decr idx;
      textbox##.value := Js.string !data.(!idx))

  let next textbox =
    if !idx < Array.length !data - 1
    then (
      Int.incr idx;
      textbox##.value := Js.string !data.(!idx))
end

let resize ~container ~textbox =
  textbox##.style##.height := Js.string "auto";
  textbox##.style##.height
  := Js.string (Printf.sprintf "%dpx" (max 18 textbox##.scrollHeight));
  container##.scrollTop := container##.scrollHeight

let text ~a_class:cl s = Tyxml_js.Html.(span ~a:[ a_class [ cl ] ] [ txt s ])

let append output cl s =
  Dom.appendChild output (Tyxml_js.To_dom.of_element (text ~a_class:cl s))

let run () =
  let toploop = Toploop.create () in
  let container = by_id "toplevel-container" in
  let output = by_id "output" in
  let textbox : 'a Js.t = by_id_coerce "userinput" Dom_html.CoerceTo.textarea in
  let sharp_chan = Stdio.Out_channel.create "/dev/null0" in
  let sharp_ppf = Caml.Format.formatter_of_out_channel sharp_chan in
  let caml_chan = Stdio.Out_channel.create "/dev/null1" in
  let caml_ppf = Caml.Format.formatter_of_out_channel caml_chan in
  let execute () =
    let content = Js.to_string textbox##.value##trim in
    textbox##.value := Js.string "";
    History.push content;
    Toploop.execute toploop ~pp_code:sharp_ppf ~pp:caml_ppf content;
    resize ~container ~textbox;
    container##.scrollTop := container##.scrollHeight;
    textbox##focus
  in
  let history_down _e =
    let txt = Js.to_string textbox##.value in
    let pos = textbox##.selectionStart in
    try
      if String.length txt = pos then raise Caml.Not_found;
      let _ = String.index_from txt pos '\n' in
      Js._true
    with
    | Caml.Not_found ->
      History.current txt;
      History.next textbox;
      Js._false
  in
  let history_up _e =
    let txt = Js.to_string textbox##.value in
    let pos = textbox##.selectionStart - 1 in
    try
      if pos < 0 then raise Caml.Not_found;
      let _ = String.rindex_from txt pos '\n' in
      Js._true
    with
    | Caml.Not_found ->
      History.current txt;
      History.previous textbox;
      Js._false
  in
  let meta e =
    let b = Js.to_bool in
    b e##.ctrlKey || b e##.altKey || b e##.metaKey
  in
  let shift e = Js.to_bool e##.shiftKey in
  (* setup handlers *)
  textbox##.onkeyup
    := Dom_html.handler (fun _ ->
           resize ~container ~textbox;
           Js._true);
  textbox##.onchange
    := Dom_html.handler (fun _ ->
           resize ~container ~textbox;
           Js._true);
  textbox##.onkeydown
    := Dom_html.handler (fun e ->
           match e##.keyCode with
           | 13 when not (meta e || shift e) ->
             resize ~container ~textbox;
             Js._true
           | 13 ->
             execute ();
             Js._false
           | 76 when meta e ->
             output##.innerHTML := Js.string "";
             Js._true
           | 38 -> history_up e
           | 40 -> history_down e
           | _ -> Js._true);
  resize ~container ~textbox;
  textbox##focus;
  Sys_js.set_channel_flusher caml_chan (append output "caml");
  Sys_js.set_channel_flusher sharp_chan (append output "sharp");
  Sys_js.set_channel_flusher Stdio.stdout (append output "stdout");
  Sys_js.set_channel_flusher Stdio.stderr (append output "stderr");
  let readline () =
    Js.Opt.case
      (Dom_html.window##prompt (Js.string "The toplevel expects inputs:") (Js.string ""))
      (fun () -> "")
      (fun s -> Js.to_string s ^ "\n")
  in
  Sys_js.set_channel_filler Stdio.stdin readline;
  History.setup ();
  textbox##.value := Js.string ""

let () =
  Dom_html.window##.onload
    := Dom_html.handler (fun _ ->
           run ();
           Js._false)
