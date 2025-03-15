%{

open Raw

let plus, star, qmark =
  let span = Lexing.dummy_pos, Lexing.dummy_pos in
  let sym data = { span; data = NTerm data } in
  sym "nonempty_list", sym "list", sym "option"
;;

%}

%start<Raw.t> grammar

%token<string> ID TID TYPE
%token<Raw.code> CODE
%token<string> DCODE
%token DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN
%token DINLINE DPREC
%token BAR COLON COMMA EQ PLUS QMARK SEMI STAR LPAREN RPAREN
%token EOF

%%

grammar:
  | r_decls=decl* DSEP r_rules=rule* EOF { { r_decls; r_rules } }
;

decl:
  | data=node(DCODE)         { DeclCode data }
  | DTOKEN tp=tp? xs=tid*    { DeclToken (tp, xs) }
  | DSTART tp=tp? xs=id*     { DeclStart (tp, xs) }
  | DTYPE  tp=tp  xs=symbol* { DeclType  (tp, xs) }
  | DLEFT         xs=ident*  { DeclLeft xs }
  | DRIGHT        xs=ident*  { DeclRight xs }
  | DNONASSOC     xs=ident*  { DeclNonassoc xs }
;

rule:
  | r_inline=boption(DINLINE)
    r_id=id r_params=loption(parameters) COLON
    option(BAR) r_prods=separated_nonempty_list(BAR, production)
    SEMI*
      { { r_id; r_inline; r_params; r_prods } }
;

parameters:
  | LPAREN params=separated_list(COMMA, parameter) RPAREN { params }
;

%inline
parameter:
  | x=symbol { x }
;

production:
  | p_prod=producer*
    p_prec=preceded(DPREC, ident)?
    p_actions=actions
      { { p_prod; p_prec; p_actions } }
;

actions:
  | xs=conditional_action+             { xs }
  | xs=conditional_action* a_code=code { xs @ [{ a_cond = None; a_code }] }
;

conditional_action:
  | DWHEN lhs=symbol EQ rhs=symbol a_code=code { { a_cond = Some ((lhs, rhs)); a_code } }
;

producer:
  | p_id=ioption(terminated(id, EQ))
    p_actual=actual
    SEMI*
      { { p_id; p_actual } }
;

actual:
  | a_actual=actual a_symbol=shorthand   { { a_symbol; a_args = [ Arg a_actual ] } }
  | a_symbol=symbol a_args=loption(args) { { a_symbol; a_args } }
;

shorthand:
  | PLUS  { plus }
  | STAR  { star }
  | QMARK { qmark }
;

args:
  | LPAREN args=separated_nonempty_list(COMMA, arg) RPAREN { args }
;

arg:
  | x=actual                       { Arg x }
  | a_prod=producer* a_action=code { ArgInline { a_prod; a_action } }
;

symbol:
  | x=ID  { { span = $loc; data = NTerm x } }
  | x=TID { { span = $loc; data = Term x } }
;

ident:
  | x=node(ID)  { x }
  | x=node(TID) { x }
;

%inline id:   node(ID)   { $1 };
%inline tid:  node(TID)  { $1 };
%inline tp:   node(TYPE) { $1 };
%inline code: node(CODE) { $1 };

%inline node(X): data=X { { span = $loc; data } };
