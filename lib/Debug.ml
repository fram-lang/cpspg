let string_of_pos p = Printf.sprintf "%d:%d" p.Lexing.pos_lnum (p.pos_cnum - p.pos_bol)
let string_of_span (l, r) = Printf.sprintf "%s-%s" (string_of_pos l) (string_of_pos r)

let string_of_kw = function
  | Raw.KwArg i -> Printf.sprintf "$%d" i
  | Raw.KwStartpos -> "$startpos"
  | Raw.KwEndpos -> "$endpos"
  | Raw.KwSymbolstartpos -> "$symbolstartpos"
  | Raw.KwStartofs -> "$startofs"
  | Raw.KwEndofs -> "$endofs"
  | Raw.KwSymbolstartofs -> "$symbolstartofs"
  | Raw.KwLoc -> "$loc"
  | Raw.KwSloc -> "$sloc"
;;

let string_of_token = function
  | Parser.BAR -> "|"
  | Parser.CODE (code, kw) ->
    let string_of_kw (kw, l, r) =
      Printf.sprintf ", %s %s" (string_of_kw kw) (string_of_span (l, r))
    in
    Printf.sprintf "CODE(%S%s)" code (List.map string_of_kw kw |> String.concat "")
  | Parser.COLON -> ":"
  | Parser.COMMA -> ","
  | Parser.DCODE s -> Printf.sprintf "DCODE(%S)" s
  | Parser.DINLINE -> "%inline"
  | Parser.DLEFT -> "%left"
  | Parser.DNONASSOC -> "%nonassoc"
  | Parser.DPREC -> "%prec"
  | Parser.DRIGHT -> "%right"
  | Parser.DSEP -> "%%"
  | Parser.DSTART -> "%start"
  | Parser.DTOKEN -> "%token"
  | Parser.DTYPE -> "%type"
  | Parser.DWHEN -> "%when"
  | Parser.EOF -> "EOF"
  | Parser.EQ -> "="
  | Parser.ID s -> Printf.sprintf "ID(%s)" s
  | Parser.LPAREN -> "("
  | Parser.PLUS -> "+"
  | Parser.QMARK -> "?"
  | Parser.RPAREN -> ")"
  | Parser.SEMI -> ";"
  | Parser.STAR -> "*"
  | Parser.TID s -> Printf.sprintf "TID(%s)" s
  | Parser.TYPE s -> Printf.sprintf "TYPE(%s)" s
;;

let debug_token token lexbuf =
  Printf.eprintf
    "(%s) %s\n%!"
    (string_of_span (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p))
    (string_of_token token)
;;
