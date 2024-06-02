%{
let fail () = Parsing.error "arithmetic error"

let pow {`re : {type X} -> Unit ->[|_] X} =
    let rec aux a (n : Int) = 
	if n == 0 then 1
	    else if n == 1 then a
	    else (let (b : Int) = aux a (n / 2) in
		  b * b * (if n % 2 == 0 then 1 else a))
    in aux

%}

%token<Int> INT
%token PLUS MINUS SLASH STAR PERCENT CARET LPAREN RPAREN EOF
%start<Int> main

%%

main: x=expr EOF { x };

expr:
    | l=expr PLUS  r=term { let (l : Int) = l in l + r }
    | l=expr MINUS r=term { let (l : Int) = l in l - r }
    | x=term                    { x }
;

term:
    | l=term STAR    r=factor { let (l : Int) = l in l * r }
    | l=term SLASH   r=factor { let (l : Int) = l in let `re = fail in l / r }
    | l=term PERCENT r=factor { let (l : Int) = l in let `re = fail in l % r }
    | x=factor                { x }
;

factor:
    | l=base CARET r=factor { let `re = fail in pow l r }
    | x=base                { x }
;

base:
    | x=INT                      { x }
    | LPAREN x=expr RPAREN { x }
;
