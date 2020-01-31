open Base

module Error : sig
  type t =
    { message : string
    ; context : string option
    }
  [@@deriving sexp]
end

val token_to_string : Parser.token -> string
val tokens : filename:string -> Parser.token list
val parse : filename:string -> (Mini_ast.t, Error.t) Result.t
val ok_exn : ('a, Error.t) Result.t -> 'a
