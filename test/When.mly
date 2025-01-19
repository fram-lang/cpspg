%token EOF
%token WILDCARD EQ
%token<string> IDENT
%token<int> INT
%token KW_LET
%token LP RP

%start<term> program

%{
  type pat =
    | PWildcard
    | PIdent of string
    | PTuple of pat list

  type expr =
    | EInt of int
    | EIdent of string
    | ETuple of expr list

  type term = TLet of pat * expr
%}

%%

program: term=term EOF { term };

term:
  | KW_LET pat=pat EQ expr=expr { TLet (pat, expr) }
;

pat:  expr_or_pat(pat)  { $1 };
expr: expr_or_pat(expr) { $1 };

expr_or_pat(kind):
  (* Common *)
  | id=IDENT
      %when kind = expr { EIdent id }
      %when kind = pat  { PIdent id }
  | LP xs=kind* RP
      %when kind = expr { ETuple xs }
      %when kind = pat  { PTuple xs }
  (* Expresisons *)
  | x=INT    %when kind = expr { EInt x }
  (* Patterns *)
  | WILDCARD %when kind = pat  { PWildcard }
;
