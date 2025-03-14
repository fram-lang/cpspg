{

open Raw
open Parser
open Lexing

exception UnexpectedInput of (char option)

let buf = Buffer.create 256
let add_char = Buffer.add_char buf
let add_string = Buffer.add_string buf

let add_newline lexbuf =
  Lexing.new_line lexbuf;
  add_char '\n'
;;

(** [add_lexeme lexbuf] is identical to [add_string (Lexing.lexeme lexbuf)] *)
let add_lexeme lexbuf =
  let l = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
  Buffer.add_subbytes buf lexbuf.lex_buffer lexbuf.lex_start_pos l
;;

let wrap_string_lexer f lexbuf =
  let s = lexbuf.lex_start_p in
  Buffer.clear buf;
  f lexbuf;
  lexbuf.lex_start_p <- s;
  Buffer.contents buf
;;

let keyword_of_string = function
  | "$startpos" -> KwStartpos
  | "$endpos" -> KwEndpos
  | "$symbolstartpos" -> KwSymbolstartpos
  | "$startofs" -> KwStartofs
  | "$endofs" -> KwEndofs
  | "$symbolstartofs" -> KwSymbolstartofs
  | "$loc" -> KwLoc
  | "$sloc" -> KwSloc
  | _ -> assert false
;;

}

let newline = '\r'* '\n'
let blank = [' ' '\009' '\012']

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule main = parse
  | newline { new_line lexbuf; main lexbuf }
  | blank   { main lexbuf }

  | '#' blank* (['0'-'9']+ as num) blank* ('"' ([^ '\r' '\n' '"']* as name) '"')? [^ '\r' '\n']* newline
    { let pos = lexbuf.lex_curr_p in
      let name = Option.value ~default:pos.pos_fname name in
      lexbuf.lex_curr_p <- { pos with pos_fname = name; pos_lnum = (int_of_string num); pos_bol = pos.pos_cnum };
      main lexbuf }

  | "(*" { comment 0 lexbuf; main lexbuf }

  | "/*" { c_comment lexbuf; main lexbuf }
  | "//" { c_line_comment lexbuf; main lexbuf }

  | "%token"    { DTOKEN }
  | "%term"     { DTOKEN }
  | "%type"     { DTYPE }
  | "%start"    { DSTART }
  | "%left"     { DLEFT }
  | "%right"    { DRIGHT }
  | "%nonassoc" { DNONASSOC }
  | "%binary"   { DNONASSOC }
  | "%%"        { DSEP }
  | "%{"        { DCODE (wrap_string_lexer (dcode 0) lexbuf) }

  | "%inline"   { DINLINE }
  | "%prec"     { DPREC }
  | "%when"     { DWHEN }

  | "%\\" { DSEP }
  | "%<"  { DLEFT }
  | "%>"  { DRIGHT }
  | "%0"  { DTOKEN }
  | "%2"  { DNONASSOC }

  | '|' { BAR }
  | ':' { COLON }
  | ',' { COMMA }
  | '=' { EQ }
  | '+' { PLUS }
  | '?' { QMARK }
  | ';' { SEMI }
  | '*' { STAR }
  | '(' { LPAREN }
  | ')' { RPAREN }

  | lowercase identchar* as i { ID i }
  | uppercase identchar* as i { TID i }

  | '<' { TYPE (wrap_string_lexer (tag 0) lexbuf) }

  | '{'
    { Buffer.clear buf;
      let s = lexbuf.lex_start_p
      and kw  = code 0 [] lexbuf in
      lexbuf.lex_start_p <- s;
      CODE (Buffer.contents buf, kw) }

  | eof    { EOF }
  | _ as c { raise (UnexpectedInput (Some c)) }

and tag depth = parse
  | '[' | '(' { add_lexeme lexbuf; tag (depth + 1) lexbuf }
  | ']' | ')' { add_lexeme lexbuf; tag (depth - 1) lexbuf }

  | "->" { add_lexeme lexbuf; tag depth lexbuf }
  | '>'  { if depth > 0 then (add_lexeme lexbuf; tag depth lexbuf) }

  | newline { add_newline lexbuf; tag depth lexbuf }
  | _       { add_lexeme  lexbuf; tag depth lexbuf }
  | eof     { raise (UnexpectedInput None) }

and code depth kw = parse
  | '[' | '(' | '{' { add_lexeme lexbuf; code (depth + 1) kw lexbuf }
  | ']' | ')'       { add_lexeme lexbuf; code (depth - 1) kw lexbuf }
  | '}'             { if depth > 0 then (add_lexeme lexbuf; code (depth - 1) kw lexbuf) else List.rev kw }

  | "$startpos"
  | "$endpos"
  | "$symbolstartpos"
  | "$startofs"
  | "$endofs"
  | "$symbolstartofs"
  | "$loc"
  | "$sloc" as k
    { add_lexeme lexbuf;
      let k = keyword_of_string k, lexbuf.lex_start_p, lexbuf.lex_curr_p in
      code depth (k :: kw) lexbuf }

  | '$' (['0'-'9']+ as i)
    { add_lexeme lexbuf;
      let k = KwArg (int_of_string i), lexbuf.lex_start_p, lexbuf.lex_curr_p in
      code depth (k :: kw) lexbuf }

  | '"'  { add_lexeme lexbuf; string lexbuf;    add_lexeme lexbuf; code depth kw lexbuf }
  | "(*" { add_lexeme lexbuf; comment 0 lexbuf; add_lexeme lexbuf; code depth kw lexbuf }

  | newline { add_newline lexbuf; code depth kw lexbuf }
  | _       { add_lexeme lexbuf;  code depth kw lexbuf }
  | eof     { raise (UnexpectedInput None) }

and dcode depth = parse
  | "%}"            { if depth > 0 then (add_lexeme lexbuf; dcode (depth - 1) lexbuf) }
  | '[' | '(' | '{' { add_lexeme lexbuf; dcode (depth + 1) lexbuf }
  | ']' | ')' | '}' { add_lexeme lexbuf; dcode (depth - 1) lexbuf }

  | '"'  { add_lexeme lexbuf; string lexbuf;    add_lexeme lexbuf; dcode depth lexbuf }
  | "(*" { add_lexeme lexbuf; comment 0 lexbuf; add_lexeme lexbuf; dcode depth lexbuf }

  | newline { add_newline lexbuf; dcode depth lexbuf }
  | _       { add_lexeme  lexbuf; dcode depth lexbuf }
  | eof     { raise (UnexpectedInput None) }

and string = parse
  | '"'     { }
  | "\\\\"  { add_char '\\'; string lexbuf }
  | "\\\""  { add_char '\"'; string lexbuf }
  | newline { add_newline lexbuf; string lexbuf }
  | _       { add_lexeme  lexbuf; string lexbuf }
  | eof     { raise (UnexpectedInput None) }

and comment depth = parse
  | "(*" { add_lexeme lexbuf; comment (depth + 1) lexbuf }
  | "*)" { if depth > 0 then (add_lexeme lexbuf; comment (depth - 1) lexbuf) }

  | '"' { add_lexeme lexbuf; string lexbuf; add_lexeme lexbuf; comment depth lexbuf }

  | newline  { add_newline lexbuf; comment depth lexbuf }
  | _        { add_lexeme  lexbuf; comment depth lexbuf }
  | eof      { raise (UnexpectedInput None) }

(* C-style block comments - for ocamlyacc compability *)
and c_comment = parse
  | "*/" { }
  | eof  { raise (UnexpectedInput None) }
  | _    { c_comment lexbuf }

(* C-style line comments - for ocamlyacc compability *)
and c_line_comment = parse
  | "\n" { }
  | eof  { raise (UnexpectedInput None) }
  | _    { c_line_comment lexbuf }
