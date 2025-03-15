type token =
  | BAR
  | CODE of (Raw.code)
  | COLON
  | COMMA
  | DCODE of (string)
  | DINLINE
  | DLEFT
  | DNONASSOC
  | DPREC
  | DRIGHT
  | DSEP
  | DSTART
  | DTOKEN
  | DTYPE
  | DWHEN
  | EOF
  | EQ
  | ID of (string)
  | LPAREN
  | PLUS
  | QMARK
  | RPAREN
  | SEMI
  | STAR
  | TID of (string)
  | TYPE of (string)
;;

val error_token : unit -> token option
val expected_tokens : unit -> string list

val grammar : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Raw.t
