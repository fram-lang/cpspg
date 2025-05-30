(* comment *)

%{
 (* Comment *)
 "string"
 "string\""
 "string\\"
 "string\\\""
%}

%token<unit -> unit> UU

%%

start:
 | { }
 | { () }
 | { {} }
 | { (* comment *) }
 | { "string" }
 | { "string}" }
 | { $loc }
 | { "a" $1 "b" $2 "c" $3 }
 | { let (p, n) = $1 in p, n }
;
