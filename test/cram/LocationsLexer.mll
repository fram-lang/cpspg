{
  open LocationsParser
}

rule token = parse
  | '\n'             { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']+      { token lexbuf }

  | "endpos"         { ENDPOS }
  | "startpos"       { STARTPOS }
  | "loc"            { LOC }

  | eof              { EOF }
  | _                { assert false }
