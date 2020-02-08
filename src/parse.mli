open Base

module Error : sig
  type t =
    { message : string
    ; context : string option
    }
  [@@deriving sexp]
end

val token_to_string : Parser.token -> string
val tokens_string : string -> Parser.token list
val tokens_file : string -> Parser.token list
val parse : ?filename:string -> Stdio.In_channel.t -> (Ast.t, Error.t) Result.t
val parse_string : ?filename:string -> string -> (Ast.t, Error.t) Result.t
val parse_file : string -> (Ast.t, Error.t) Result.t
val ok_exn : ('a, Error.t) Result.t -> 'a
