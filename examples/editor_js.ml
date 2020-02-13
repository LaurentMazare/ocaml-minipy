open Base
open Js_of_ocaml
module I = Minipy.Interpreter
module Parse = Minipy.Parse
module Format = Caml.Format
open Js_of_ocaml_tyxml

let protect ~f =
  try f () with
  | I.RuntimeError message -> Stdio.eprintf "RuntimeError: %s\n%!" message

let eval_stmts env stmts = protect ~f:(fun () -> I.eval_stmts env stmts)
let by_id s = Dom_html.getElementById s

let by_id_coerce s f =
  Js.Opt.get (f (Dom_html.getElementById s)) (fun () -> raise Caml.Not_found)

let title_prefix = "## "
let text ~a_class:cl s = Tyxml_js.Html.(span ~a:[ a_class [ cl ] ] [ txt s ])

let rec iter_on_sharp ~f x =
  Js.Opt.iter (Dom_html.CoerceTo.element x) (fun e ->
      if Js.to_bool (e##.classList##contains (Js.string "sharp")) then f e);
  match Js.Opt.to_option x##.nextSibling with
  | None -> ()
  | Some n -> iter_on_sharp ~f n

let do_by_id s f =
  try f (Dom_html.getElementById s) with
  | Caml.Not_found -> ()

let setup_clear_button ~output =
  do_by_id "btn-clear" (fun e ->
      e##.style##.display := Js.string "block";
      e##.onclick
        := Dom_html.handler (fun _ ->
               output##.innerHTML := Js.string "";
               Js._false))

let setup_exec_button ~execute =
  do_by_id "btn-exec" (fun e ->
      e##.style##.display := Js.string "block";
      e##.onclick
        := Dom_html.handler (fun _ ->
               execute ();
               Js._false))

let append output cl s =
  Dom.appendChild output (Tyxml_js.To_dom.of_element (text ~a_class:cl s))

let run () =
  let editor_id = by_id "editor" in
  let editor = (Js.Unsafe.js_expr "ace")##edit (Js.string "editor") in
  ignore (editor##.session##setMode (Js.string "ace/mode/python"));
  let output = by_id "output" in
  let execute () =
    let content = Js.to_string (Js.Unsafe.meth_call editor "getValue" [||])##trim in
    let env = I.Env.empty ~builtins:I.default_builtins in
    let stmts = Parse.parse_string (content ^ "\n") in
    (match stmts with
    | Error { message; context } ->
      Stdio.eprintf "ParseError: %s\n%!" message;
      Option.iter context ~f:(fun c -> Stdio.eprintf "%s\n%!" c)
    | Ok stmts -> eval_stmts env stmts);
    editor_id##focus
  in
  let meta e =
    let b = Js.to_bool in
    b e##.ctrlKey || b e##.altKey || b e##.metaKey
  in
  let shift e = Js.to_bool e##.shiftKey in
  Sys_js.set_channel_flusher Stdio.stdout (append output "stdout");
  Sys_js.set_channel_flusher Stdio.stderr (append output "stderr");
  editor_id##.onkeydown
    := Dom_html.handler (fun e ->
           match e##.keyCode with
           | 13 when not (meta e || shift e) -> Js._true
           | 13 ->
             execute ();
             Js._false
           | 76 when meta e ->
             output##.innerHTML := Js.string "";
             Js._true
           | _ -> Js._true);
  editor_id##focus;
  setup_clear_button ~output;
  setup_exec_button ~execute

let () =
  Dom_html.window##.onload
    := Dom_html.handler (fun _ ->
           run ();
           Js._false)
