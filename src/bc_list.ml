open Base
open Import

let builtin name fn = Bc_value.Builtin_fn { name; fn = (fun _eval -> fn) }

let attrs queue ~attr =
  match attr with
  | "append" ->
    let append args kwargs =
      check_empty_kwargs kwargs ~name:"append";
      match args with
      | [ a ] ->
        Queue.enqueue queue a;
        Bc_value.none
      | _ -> errorf "append expects a single argument, got %d" (List.length args)
    in
    builtin "append" append
  | "clear" ->
    let clear args kwargs =
      check_empty_kwargs kwargs ~name:"clear";
      if not (List.is_empty args) then errorf "clear expects no argument";
      Queue.clear queue;
      Bc_value.none
    in
    builtin "clear" clear
  | "pop" ->
    let pop args kwargs =
      check_empty_kwargs kwargs ~name:"pop";
      if not (List.is_empty args) then errorf "pop expects no argument";
      match Queue.dequeue queue with
      | None -> errorf "pop from empty list"
      | Some v -> v
    in
    builtin "pop" pop
  | "reverse" ->
    let reverse args kwargs =
      check_empty_kwargs kwargs ~name:"reverse";
      if not (List.is_empty args) then errorf "reverse expects no argument";
      let result = Queue.create () in
      let nelems = Queue.length queue in
      for i = 0 to nelems - 1 do
        Queue.enqueue result (Queue.get queue (nelems - i - 1))
      done;
      Queue.clear queue;
      Queue.blit_transfer ~src:result ~dst:queue ();
      Bc_value.none
    in
    builtin "reverse" reverse
  | "sort" ->
    let sort args kwargs =
      check_empty_kwargs kwargs ~name:"sort";
      if not (List.is_empty args) then errorf "reverse expects no argument";
      let sorted = Queue.to_array queue in
      Array.sort sorted ~compare:Poly.compare;
      Queue.clear queue;
      for i = 0 to Array.length sorted - 1 do
        Queue.enqueue queue sorted.(i)
      done;
      Bc_value.none
    in
    builtin "sort" sort
  | attr -> errorf "'list' object has no attribute '%s'" attr
