module IntMap = Map.Make (Int)
module SymbolMap = Map.Make (Automaton.Symbol)

let dummy_span = Lexing.dummy_pos, Lexing.dummy_pos
let verbatim data = { Raw.span = dummy_span; data = String.trim data }

let rec rev_mapi f i acc = function
  | [] -> acc
  | x :: xs -> rev_mapi f (i + 1) (f i x :: acc) xs
;;

let filter_mapi f xs =
  let rec aux i = function
    | [] -> []
    | x :: xs ->
      (match f i x with
       | None -> aux (i + 1) xs
       | Some v -> v :: aux (i + 1) xs)
  in
  aux 0 xs
;;

(* -unused-rec-flag due continuations always being mutually recursive, while often they don't need to *)
(* FIXME: should we include -redunant-{case, subpat}? They trigger warnings
   in grammars with unresolved conflicts, but maybe it's a good thing? *)
let prelude = {|
[@@@warning "-unused-rec-flag"]
[@@@warning "-redundant-case"]
[@@@warning "-redundant-subpat"]
|} |> verbatim
[@@ocamlformat "disable"]

let parsing_compat = {|
module Parsing : sig
  include module type of Stdlib.Parsing
  val set_loc : (Lexing.position * Lexing.position) list -> int -> unit
end = struct
  include Stdlib.Parsing

  let action_loc = ref []
  let action_len = ref 0

  let set_loc loc n =
    action_loc := loc;
    action_len := n

  let symbol_start_pos _ =
    let rec take_rev acc k = function
      | _ when k = 0 -> acc
      | [] -> acc
      | x :: xs -> take_rev (x :: acc) (k - 1) xs
    and loop = function
      | [] -> Lexing.dummy_pos
      | [ (st, _) ] -> st
      | (st, en) :: _ when st <> en -> st
      | _ :: loc -> loop loc
    in
    loop (take_rev [] !action_len !action_loc)
  ;;

  let symbol_end_pos _ =
    match !action_loc with
    | (_, en) :: _ -> en
    | [] -> Lexing.dummy_pos
  ;;

  let rhs_start_pos n = List.nth !action_loc (!action_len - (n - 1)) |> fst
  let rhs_end_pos n = List.nth !action_loc (!action_len - (n - 1)) |> snd
  let symbol_start () = (symbol_start_pos ()).pos_cnum
  let symbol_end () = (symbol_end_pos ()).pos_cnum
  let rhs_start n = (rhs_start_pos n).pos_cnum
  let rhs_end n = (rhs_end_pos n).pos_cnum

  let stub name =
    failwith ("function " ^ name ^ " is not included in the compatibility module")
  ;;

  let clear_parser _ = stub "clear_parser"
  let set_trace _ = stub "set_trace"
  let yyparse _ _ _ _ = stub "yyparse"
  let peek_val _ _ = stub "peek_val"
  let is_current_lookahead _ = stub "is_current_lookahead"
  let parse_error _ = stub "parse_error"
end

module ParsingCompat = Parsing
|} |> verbatim
[@@ocamlformat "disable"]

let parsing_compat_stub = {|
module ParsingCompat = struct
  let set_loc _ _ = ()
end
|} |> verbatim
[@@ocamlformat "disable"]

let action_lib = {|
let _kw_endpos ~_loc _ =
  match _loc with
  | l :: _ -> snd l
  | [] -> Lexing.dummy_pos
;;

let _kw_startpos ~_loc n =
  match List.nth_opt _loc (n - 1) with
  | Some l -> fst l
  | None -> _kw_endpos ~_loc n
;;

let _kw_symbolstartpos ~_loc _ = failwith "not implemented: $symbolstartpos"
let _kw_startofs ~_loc _ = failwith "not implemented: $startofs"
let _kw_endofs ~_loc _ = failwith "not implemented: $endofs"
let _kw_symbolstartofs ~_loc _ = failwith "not implemented: $symbolstartofs"
let _kw_loc ~_loc n = _kw_startpos ~_loc n, _kw_endpos ~_loc n
let _kw_sloc ~_loc _ = failwith "not implemented: $sloc"
|} |> verbatim
[@@ocamlformat "disable"]

let state_lib = {|
let lexfun = ref (fun _ -> assert false)
let lexbuf = ref (Lexing.from_string String.empty)

let error_token = ref None
let expected_tokens = ref []

let setup lf lb =
  lexfun := lf;
  lexbuf := lb;
  error_token := None;
  expected_tokens := []
;;

let shift () =
  let tok = !lexfun !lexbuf in
  let loc = !lexbuf.lex_start_p, !lexbuf.lex_curr_p in
  tok, loc
;;

let fail token expected =
  error_token := Some (fst token);
  expected_tokens := expected;
  raise Parsing.Parse_error
;;

let loc_shift ~_loc l = l :: _loc

let loc_dummy = function
  | [] -> Lexing.dummy_pos, Lexing.dummy_pos
  | (_, e) :: _ -> e, e
;;

let loc_reduce ~_loc = function
  | 0 -> loc_dummy _loc :: _loc
  | n ->
    let l = fst (List.nth _loc (n - 1)), snd (List.hd _loc) in
    ParsingCompat.set_loc _loc n;
    l :: List.drop n _loc
;;
|} |> verbatim
[@@ocamlformat "disable"]

let epilogue = {|
let error_token () = !States.error_token
let expected_tokens () = !States.expected_tokens
|} |> verbatim
[@@ocamlformat "disable"]

module Make (S : Types.BackEndSettings) (G : Types.Grammar) (A : Types.Automaton) :
  Types.Code = struct
  open Automaton
  module PP = Printer.Ml (S)
  module D = Dot.Make (S) (G) (A)

  (* Utils *)

  let term_name t = (G.term t).ti_name.data
  let nterm_name n = (G.nterm n).ni_name.data

  let symbol_name = function
    | Term t -> term_name t
    | NTerm n -> nterm_name n
  ;;

  let symbol_has_value = function
    | NTerm _ -> true
    | Term t -> (G.term t).ti_ty |> Option.is_some
  ;;

  (* Identifiers *)

  let arg_id symbol idx =
    if S.readable_ids
    then Printf.sprintf "a%d_%s" idx (symbol_name symbol)
    else Printf.sprintf "a%d" idx
  ;;

  (* Continuations are prefixed with underscore because
   precedence declarations could make them unused
   (see unary minus in `calc/ParserPres.mly`) *)
  let cont_id group idx =
    match S.readable_ids, group.g_starting with
    | false, _ -> Printf.sprintf "_c%d" idx
    | true, false -> Printf.sprintf "_c%d_%s" idx (nterm_name group.g_symbol)
    | true, true -> Printf.sprintf "_c%d_%s_starting" idx (nterm_name group.g_symbol)
  ;;

  (* TODO: Include rule name in action name when [S.readable_ids] is enabled *)
  let semantic_action_id _action idx = Printf.sprintf "a%d" idx
  let state_id idx = Printf.sprintf "%s%d" (if S.readable_ids then "state_" else "s") idx

  (* Helpers *)

  let with_loc args = if S.locations then PP.ExprLabeled "_loc" :: args else args

  let make_vars vars expr =
    let binding (name, valu) = PP.{ name; args = []; expr = valu; comment = None } in
    let bindings = List.map binding vars in
    PP.ExprLet (PP.NonRecursive, bindings, expr)
  ;;

  let make_var name valu expr = make_vars [ name, valu ] expr

  (* Rest *)

  let make_token_type symbols =
    let get_info = function
      | NTerm _ -> None
      | Term t -> Some (G.term t)
    and cmp a b = String.compare a.ti_name.data b.ti_name.data in
    let infos = List.filter_map get_info symbols |> List.sort cmp in
    List.map (fun i -> { PP.name = i.ti_name; contents = i.ti_ty }) infos
  ;;

  let make_semantic_action_code action =
    let n = List.length action.sa_args
    and code, keywords = action.sa_code.data in
    let s, e = action.sa_code.span in
    let make_part l r =
      let len = r.Lexing.pos_cnum - l.Lexing.pos_cnum
      and ofs = l.pos_cnum - s.pos_cnum - 1 in
      { data = String.sub code ofs len; span = l, r }
    and get_impl = function
      | Raw.KwStartpos -> Printf.sprintf "(_kw_startpos ~_loc %d)" n
      | Raw.KwEndpos -> Printf.sprintf "(_kw_endpos ~_loc %d)" n
      | Raw.KwSymbolstartpos -> Printf.sprintf "(_kw_symbolstartpos ~_loc %d)" n
      | Raw.KwStartofs -> Printf.sprintf "(_kw_startofs ~_loc %d)" n
      | Raw.KwEndofs -> Printf.sprintf "(_kw_endofs ~_loc %d)" n
      | Raw.KwSymbolstartofs -> Printf.sprintf "(_kw_symbolstartofs ~_loc %d)" n
      | Raw.KwLoc -> Printf.sprintf "(_kw_loc ~_loc %d)" n
      | Raw.KwSloc -> Printf.sprintf "(_kw_sloc ~_loc %d)" n
      | Raw.KwArg i ->
        (match List.nth_opt action.sa_args (i - 1) with
         | Some (Some a) -> a
         | Some None -> Printf.sprintf "_arg%d" i
         | None -> "()")
    in
    let rec aux pos = function
      | [] -> [ make_part pos { e with pos_cnum = e.pos_cnum - 1 } ]
      | (kw, l, r) :: kws ->
        let part = make_part pos l
        and impl = get_impl kw |> verbatim in
        part :: impl :: aux r kws
    in
    let parts = aux { s with pos_cnum = s.pos_cnum + 1 } keywords in
    PP.ExprGrouped (PP.ExprVerbatim parts)
  ;;

  let make_semantic_action (id, action) =
    let aux i = function
      | Some a -> PP.ExprId a
      | None -> PP.ExprId (Printf.sprintf "_arg%d" (i + 1))
    in
    let args = rev_mapi aux 0 [ PP.ExprUnit ] action.sa_args in
    let args = args |> with_loc in
    let expr = make_semantic_action_code action in
    PP.{ name = semantic_action_id action id; args; expr; comment = None }
  ;;

  let make_semantic_actions actions =
    let bindings = IntMap.bindings actions |> List.map make_semantic_action in
    if IntMap.cardinal actions = 0
    then PP.StructVerbatim (verbatim "(* No actions *)")
    else PP.StructLet (PP.NonRecursive, true, bindings)
  ;;

  let make_args_ids symbols =
    let f i sym = if symbol_has_value sym then Some (arg_id sym i) else None in
    filter_mapi f symbols |> List.map (fun s -> PP.ExprId s)
  ;;

  let make_cont_ids p groups =
    let iter i g = if p g then Some (cont_id g i) else None in
    filter_mapi iter groups |> List.map (fun s -> PP.ExprId s)
  ;;

  let make_goto_call state sym =
    let closure = state.s_kernel @ state.s_closure
    and callee = state_id (SymbolMap.find sym state.s_goto) in
    let tok = [ PP.ExprId "t" ]
    and valu = if symbol_has_value sym then [ PP.ExprId "x" ] else []
    and args = make_args_ids (List.find (shifts_group sym) closure).g_prefix
    and const = make_cont_ids (shifts_group sym) closure in
    PP.ExprCall (callee, tok @ valu @ args @ const |> with_loc)
  ;;

  let make_continuation state group idx =
    let sym = NTerm group.g_symbol in
    let name = cont_id group idx
    and args = [ PP.ExprId "t"; PP.ExprId "x" ] |> with_loc
    and body = make_goto_call state sym in
    PP.{ name; args; expr = body; comment = None }
  ;;

  let make_semantic_action_args call symbols =
    let rec aux c (sym, i, acc) =
      match sym, c with
      | s :: sym, None ->
        let arg = if symbol_has_value s then PP.ExprId (arg_id s i) else PP.ExprUnit in
        sym, i + 1, arg :: acc
      | sym, Some inline ->
        let action = IntMap.find inline.ac_id A.automaton.a_actions in
        let name = semantic_action_id action inline.ac_id |> Printf.sprintf "Actions.%s"
        and sym, i, args = List.fold_right aux inline.ac_args (sym, i, []) in
        let args = List.rev_append args [ PP.ExprUnit ] in
        let expr = PP.ExprGrouped (PP.ExprCall (name, args |> with_loc)) in
        sym, i, expr :: acc
      | [], _ -> assert false
    in
    let sym, _, acc = List.fold_right aux call (symbols, 0, []) in
    assert (sym = []);
    List.rev_append acc [ PP.ExprUnit ]
  ;;

  let make_semantic_action_call group { i_action = a; _ } =
    let action = IntMap.find a.ac_id A.automaton.a_actions in
    let name = semantic_action_id action a.ac_id |> Printf.sprintf "Actions.%s" in
    let args = make_semantic_action_args a.ac_args group.g_prefix |> with_loc in
    PP.ExprCall (name, args)
  ;;

  let make_action_shift state sym =
    let patterns =
      match symbol_has_value (Term sym) with
      | true -> [ term_name sym, Some "x" ]
      | false -> [ term_name sym, None ]
    in
    let shift = PP.ExprVerbatim [ verbatim "shift ()" ]
    and locs = PP.ExprVerbatim [ verbatim "loc_shift ~_loc (snd t)" ]
    and comment = if S.comments then Some " Shift " else None in
    let expr =
      let expr = make_goto_call state (Term sym) in
      let expr = make_var "t" shift expr in
      if S.locations then make_var "_loc" locs expr else expr
    in
    PP.{ patterns; cexpr = expr; ccomment = comment }
  ;;

  let make_action_reduce state lookahead i j =
    let group = List.nth (state.s_kernel @ state.s_closure) i in
    let n, item = List.length group.g_prefix, List.nth group.g_items j in
    let pattern sym =
      match symbol_has_value (Term sym) with
      | true -> term_name sym, Some "_"
      | false -> term_name sym, None
    in
    let call = make_semantic_action_call group item
    and patterns = TermSet.elements lookahead |> List.map pattern
    and comment = if S.comments then Some " Reduce " else None
    and locs = PP.ExprVerbatim [ Printf.sprintf "loc_reduce ~_loc %d" n |> verbatim ] in
    let expr =
      let args = [ PP.ExprId "t"; PP.ExprId "x" ] |> with_loc in
      let expr = PP.ExprCall (cont_id group i, args) in
      let vars = if S.locations then [ "_loc", locs; "x", call ] else [ "x", call ] in
      make_vars vars expr
    in
    PP.{ patterns; cexpr = expr; ccomment = comment }
  ;;

  let make_action state lookahead = function
    | Shift -> TermSet.to_list lookahead |> List.map (make_action_shift state)
    | Reduce (i, j) -> [ make_action_reduce state lookahead i j ]
  ;;

  let make_action_failure state =
    let failure =
      List.fold_left (fun acc (t, _) -> TermSet.union t acc) TermSet.empty state.s_action
      |> TermSet.elements
      |> List.map (fun t -> term_name t |> Printf.sprintf "%S")
      |> String.concat "; "
      |> Printf.sprintf "fail t [ %s ]"
      |> verbatim
    in
    PP.{ patterns = [ "_", None ]; cexpr = PP.ExprVerbatim [ failure ]; ccomment = None }
  ;;

  let make_actions state =
    let cases = List.concat_map (fun (l, m) -> make_action state l m) state.s_action
    and failure = make_action_failure state in
    PP.ExprMatch (PP.ExprCall ("fst", [ PP.ExprId "t" ]), cases @ [ failure ])
  ;;

  let make_starting_actions state =
    let group = List.hd state.s_kernel in
    let item = List.nth group.g_items 0 in
    let expr = PP.ExprCall (cont_id group 0, [ PP.ExprId "t"; PP.ExprId "x" ]) in
    let expr = make_var "x" (make_semantic_action_call group item) expr in
    expr
  ;;

  let make_state_body st =
    let kn = List.length st.s_kernel
    and group = List.hd st.s_kernel in
    let conts = List.mapi (fun i g -> make_continuation st g (i + kn)) st.s_closure in
    let body =
      if group.g_starting && (List.hd group.g_items).i_suffix = []
      then make_starting_actions st
      else make_actions st
    in
    PP.ExprLet (PP.Recursive, conts, body)
  ;;

  let make_state_comment state =
    let section name = function
      | c when String.trim c = "" -> ""
      | c ->
        let c = String.trim c |> String.split_on_char '\n' in
        let c = List.map (( ^ ) "\n  ") c |> String.concat "" in
        Printf.sprintf "\n%s:%s\n" name c
    in
    let ci = Format.asprintf "%a" D.fmt_state state |> section "ITEMS"
    and cs = Format.asprintf "%a" D.fmt_state_shifts state |> section "GOTO"
    and ca = Format.asprintf "%a" D.fmt_state_actions state |> section "ACTION" in
    Printf.sprintf "%s%s%s" ci cs ca
  ;;

  let make_state (id, state) =
    let name = state_id id
    and tok = [ PP.ExprId "t" ]
    and args = make_args_ids (List.hd state.s_kernel).g_prefix
    and cont = make_cont_ids (Fun.const true) state.s_kernel
    and body = make_state_body state
    and comment = if S.comments then Some (make_state_comment state) else None in
    PP.{ name; args = tok @ args @ cont |> with_loc; expr = body; comment }
  ;;

  let make_states states =
    if IntMap.cardinal states = 0
    then PP.StructVerbatim (verbatim "(* No states *)")
    else PP.StructLet (PP.Recursive, false, IntMap.bindings states |> List.map make_state)
  ;;

  let make_entry symbol id =
    let state = Printf.sprintf "States.%s" (state_id id)
    and args = [ PP.ExprId "lexfun"; PP.ExprId "lexbuf" ]
    and setup = PP.ExprVerbatim [ verbatim "States.setup lexfun lexbuf" ]
    and cont = PP.ExprVerbatim [ verbatim "(fun _ x -> x)" ] in
    let expr = PP.ExprCall (state, [ PP.ExprId "t"; cont ] |> with_loc) in
    let expr = make_var "t" (PP.ExprVerbatim [ verbatim "States.shift ()" ]) expr in
    let expr = make_var "_loc" (PP.ExprVerbatim [ verbatim "[]" ]) expr in
    let expr = PP.ExprSeq [ setup; expr ] in
    let binding = PP.{ name = nterm_name symbol; args; expr; comment = None } in
    PP.StructLet (PP.NonRecursive, false, [ binding ])
  ;;

  let make_file { a_header; a_actions; a_states; a_starting } =
    let header = List.map (fun n -> PP.StructVerbatim n) a_header
    and actions = [ PP.StructVerbatim action_lib; make_semantic_actions a_actions ]
    and states = [ PP.StructVerbatim state_lib; make_states a_states ]
    and starting = List.map (fun (sym, id) -> make_entry sym id) a_starting in
    []
    @ [ PP.StructVerbatim prelude
      ; PP.StructVerbatim (if S.compat then parsing_compat else parsing_compat_stub)
      ]
    @ header
    @ [ PP.StructType ("token", make_token_type G.symbols)
      ; PP.StructModule ("Actions", actions)
      ; PP.StructModule ("States", states)
      ]
    @ [ PP.StructVerbatim epilogue ]
    @ starting
  ;;

  let write () = make_file A.automaton |> PP.pp_structures
end
