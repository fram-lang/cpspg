%{
let fail () = Parsing.error "arithmetic error"

let rec pow {~re : {type X} -> Unit ->[|_] X} (a : Int) (n : Int) =
	if n == 0 then 1
	else if n == 1 then a
	else (let (b : Int) = pow a (n / 2) in
	b * b * (if n % 2 == 0 then 1 else a))
%}

%token<Int> INT
%token PLUS MINUS SLASH STAR PERCENT CARET LPAREN RPAREN EOF
%start<Int> main

%left PLUS MINUS
%left SLASH STAR PERCENT
%nonassoc UMINUS
%right CARET

%%

main: x=expr EOF { x };

expr:
    | l=expr PLUS    r=expr { let (l : Int) = l in l + r }
    | l=expr MINUS   r=expr { let (l : Int) = l in l - r }
    | l=expr STAR    r=expr { let (l : Int) = l in l * r }
    | l=expr SLASH   r=expr { let ~re = fail in let (l : Int) = l in l / r }
    | l=expr PERCENT r=expr { let ~re = fail in let (l : Int) = l in l % r }
    | l=expr CARET   r=expr { let ~re = fail in let (l : Int) = l in pow l r }

    | MINUS x=expr %prec UMINUS { 0 - x }

    | LPAREN x=expr RPAREN  { x }
    | x=INT                 { x }
;
