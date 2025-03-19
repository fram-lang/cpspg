%token EOF
%token ENDPOS STARTPOS LOC
%start<unit> start

%{
  open Lexing

  let pp_pos ppf pos = Format.fprintf ppf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  let pp_loc ppf loc = Format.fprintf ppf "%a-%a" pp_pos (fst loc) pp_pos (snd loc)

  let print_pos pos = Format.printf "%a\n" pp_pos pos
  let print_loc loc = Format.printf "%a\n" pp_loc loc
%}

%%

start: loc* EOF { () }

loc:
  | STARTPOS { print_pos $startpos }
  | ENDPOS   { print_pos $endpos }
  | LOC      { print_loc $loc }
;
