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
  val reset : t -> unit
  val execute : t -> pp_code:Format.formatter -> pp:Format.formatter -> string -> unit
end = struct
  type t = { mutable env : I.Env.t }

  let create () = { env = I.Env.empty () }
  let reset t = t.env <- I.Env.empty ()

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

  let eval_stmts t stmts = protect ~f:(fun () -> I.eval_stmts t.env stmts)

  let execute t ~pp_code ~pp content =
    Format.fprintf pp_code "%s\n%!" content;
    let stmts = Parse.parse_string (content ^ "\n") in
    match stmts with
    | Error { message; context } ->
      Stdio.eprintf "ParseError: %s\n%!" message;
      Option.iter context ~f:(fun c -> Stdio.eprintf "%s\n%!" c)
    | Ok stmts ->
      (match List.last stmts with
      | None -> ()
      | Some (Expr { value }) ->
        let stmts = List.drop_last_exn stmts in
        eval_stmts t stmts;
        protect ~f:(fun () ->
            let value = I.eval_expr t.env value in
            match value with
            | Val_none -> ()
            | value ->
              Minipy.Value.to_string value ~escape_special_chars:false
              |> Format.fprintf pp "%s\n%!")
      | Some _ -> eval_stmts t stmts)
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

let title_prefix = "## "

let setup_examples ~container ~textbox =
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
  let example_container = by_id "toplevel-examples" in
  List.iter examples ~f:(fun (name, content) ->
      let a =
        Tyxml_js.Html.(
          a
            ~a:
              [ a_class [ "list-group-item" ]
              ; a_onclick (fun _ ->
                    textbox##.value := (Js.string content)##trim;
                    resize ~container ~textbox;
                    textbox##focus;
                    true)
              ]
            [ txt name ])
      in
      Dom.appendChild example_container (Tyxml_js.To_dom.of_a a))

let text ~a_class:cl s = Tyxml_js.Html.(span ~a:[ a_class [ cl ] ] [ txt s ])

let append output cl s =
  Dom.appendChild output (Tyxml_js.To_dom.of_element (text ~a_class:cl s))

let parse_hash () =
  let frag = Url.Current.get_fragment () in
  Url.decode_arguments frag

let rec iter_on_sharp ~f x =
  Js.Opt.iter (Dom_html.CoerceTo.element x) (fun e ->
      if Js.to_bool (e##.classList##contains (Js.string "sharp")) then f e);
  match Js.Opt.to_option x##.nextSibling with
  | None -> ()
  | Some n -> iter_on_sharp ~f n

let do_by_id s f =
  try f (Dom_html.getElementById s) with
  | Caml.Not_found -> ()

let setup_share_button ~output =
  do_by_id "btn-share" (fun e ->
      e##.style##.display := Js.string "block";
      e##.onclick
        := Dom_html.handler (fun _ ->
               let code = ref [] in
               Js.Opt.iter
                 output##.firstChild
                 (iter_on_sharp ~f:(fun e ->
                      code
                        := Js.Opt.case e##.textContent (fun () -> "") Js.to_string
                           :: !code));
               let code_encoded = List.rev !code |> String.concat ~sep:"" |> B64.encode in
               let url =
                 match Url.Current.get () with
                 | Some (Http url) -> Url.Http { url with hu_fragment = "" }
                 | Some (Https url) -> Https { url with hu_fragment = "" }
                 | Some (File url) -> File { url with fu_fragment = "" }
                 | _ -> assert false
               in
               let frag =
                 Url.encode_arguments
                   (("code", code_encoded)
                   :: List.Assoc.remove (parse_hash ()) "code" ~equal:String.equal)
               in
               let uri = Url.string_of_url url ^ "#" ^ frag in
               Tyxml_js.Html.(
                 p [ txt "Share this url: "; a ~a:[ a_href uri ] [ txt uri ] ])
               |> Tyxml_js.To_dom.of_element
               |> Dom.appendChild output;
               Js._false))

let setup_exec_button ~execute =
  do_by_id "btn-exec" (fun e ->
      e##.style##.display := Js.string "block";
      e##.onclick
        := Dom_html.handler (fun _ ->
               execute ();
               Js._false))

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
           | 75 when meta e ->
             Toploop.reset toploop;
             Js._false
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
  setup_examples ~container ~textbox;
  setup_share_button ~output;
  setup_exec_button ~execute;
  History.setup ();
  textbox##.value := Js.string "";
  match List.Assoc.find (parse_hash ()) "code" ~equal:String.equal with
  | None -> ()
  | Some code ->
    textbox##.value := Js.string (B64.decode code);
    execute ()

let () =
  Dom_html.window##.onload
    := Dom_html.handler (fun _ ->
           run ();
           Js._false)
