open Base
open Js_of_ocaml
module I = Minipy.Interpreter
module Parse = Minipy.Parse
module Format = Caml.Format
open Js_of_ocaml_tyxml

let protect ~f =
  try f () with
  | Minipy.RuntimeError message -> Stdio.eprintf "RuntimeError: %s\n%!" message
  | Minipy.Value.Raise { exc; cause } ->
    let exc = Option.map exc ~f:Minipy.Value.to_string in
    let cause = Option.map cause ~f:Minipy.Value.to_string in
    Stdio.eprintf
      "%s: %s\n%!"
      (Option.value exc ~default:"unk")
      (Option.value cause ~default:"")
  | exn -> Stdio.printf "uncaught exception:\n%s\n%!" (Exn.to_string exn)

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

let setup_examples ~editor =
  let select : 'a Js.t = by_id_coerce "examplelist" Dom_html.CoerceTo.select in
  let examples =
    try
      Stdio.In_channel.read_lines "/static/examples.py"
      |> List.group ~break:(fun _ -> String.is_prefix ~prefix:title_prefix)
      |> List.filter_map ~f:(function
             | [] -> None
             | title :: _ as block ->
               let title =
                 String.chop_prefix title ~prefix:title_prefix
                 |> Option.value ~default:""
                 |> String.strip
               in
               Some (title, String.concat block ~sep:"\n"))
    with
    | _ -> []
  in
  List.iter examples ~f:(fun (name, _) ->
      Dom.appendChild select Tyxml_js.(Html.(option (txt name)) |> To_dom.of_element));
  let set_editor code = ignore (editor##setValue (Js.string code) (Js.float 1.)) in
  (match examples with
  | [] -> ()
  | (_name, example) :: _ -> set_editor example);
  select##.onchange
    := Dom_html.handler (fun _ ->
           let i = select##.selectedIndex in
           if i >= 0 && i < List.length examples
           then set_editor (snd (List.nth_exn examples i));
           ignore editor##focus;
           Js._false)

let run () =
  let editor_id = by_id "editor" in
  let editor = (Js.Unsafe.js_expr "ace")##edit (Js.string "editor") in
  ignore (editor##.commands##removeCommand (Js.string "gotoline"));
  ignore (editor##.session##setMode (Js.string "ace/mode/python"));
  let output = by_id "output" in
  let execute () =
    let content = Js.to_string (Js.Unsafe.meth_call editor "getValue" [||])##trim in
    let env = I.Env.empty () in
    let stmts = Parse.parse_string (content ^ "\n") in
    (match stmts with
    | Error { message; context } ->
      Stdio.eprintf "ParseError: %s\n%!" message;
      Option.iter context ~f:(fun c -> Stdio.eprintf "%s\n%!" c)
    | Ok stmts ->
      (match List.last stmts with
      | None -> ()
      | Some { value = Expr { value }; loc = _ } ->
        let stmts = List.drop_last_exn stmts in
        eval_stmts env stmts;
        protect ~f:(fun () ->
            let value = I.eval_expr env value in
            match value with
            | Val_none -> ()
            | value ->
              Minipy.Value.to_string value ~escape_special_chars:false
              |> Stdio.printf "%s\n%!")
      | Some _ -> eval_stmts env stmts));
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
             Js._false
           | _ -> Js._true);
  setup_examples ~editor;
  setup_clear_button ~output;
  setup_exec_button ~execute;
  ignore (editor##focus ())

let () =
  Dom_html.window##.onload
    := Dom_html.handler (fun _ ->
           run ();
           Js._false)
