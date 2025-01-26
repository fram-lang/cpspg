type loc = Lexing.position * Lexing.position

type 'a node =
  { loc : loc
  ; data : 'a
  }

type keyword =
  | KwArg of int
  | KwStartpos
  | KwEndpos
  | KwSymbolstartpos
  | KwStartofs
  | KwEndofs
  | KwSymbolstartofs
  | KwLoc
  | KwSloc

type code = string * (keyword * loc) list

type symbol =
  | NTerm of string
  | Term of string

type decl =
  | DeclToken of string node option * string node list
  | DeclStart of string node option * string node list
  | DeclType of string node * symbol node list
  | DeclLeft of string node list
  | DeclRight of string node list
  | DeclNonassoc of string node list
  | DeclCode of string node

type actual =
  { a_symbol : symbol node
  ; a_args : arg list
  }

and arg =
  | Arg of actual
  | ArgInline of
      { a_prod : producer list
      ; a_action : code node
      }

and producer =
  { p_id : string node option
  ; p_actual : actual
  }

type action =
  { a_cond : (symbol node * symbol node) option
  ; a_code : code node
  }

type production =
  { p_prod : producer list
  ; p_prec : string node option
  ; p_actions : action list
  }

type rule =
  { r_id : string node
  ; r_inline : bool
  ; r_params : symbol node list
  ; r_prods : production list
  }

type t =
  { r_decls : decl list
  ; r_rules : rule list
  }

let dummy = { r_decls = []; r_rules = [] }
