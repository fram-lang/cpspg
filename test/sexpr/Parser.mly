%{
pub data rec SExpr = Nil
           | Atom of String 
           | Cons of SExpr, SExpr
            
%}
%token<String> ATOM
%token LPAREN RPAREN DOT EOF
%start<SExpr> main

%%

main: x=s EOF { x };

s:
    | LPAREN RPAREN             { Nil }
    | x=ATOM                    { Atom x }
    | LPAREN x=s DOT y=s RPAREN { Cons x y }
;
