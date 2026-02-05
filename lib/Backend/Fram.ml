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

let prelude = {|
import Parsing
import List
|} |> verbatim
[@@ocamlformat "disable"]

let action_lib = {|
parameter ~loc
parameter E_err
parameter ~error : Parsing.Error E_err

pub let _kw_endpos _ =
  match ~loc with
  | l :: _ => snd l
  | [] => Parsing.dummyPos
  end

pub let _kw_startpos (n : Int) =
  match List.nth ~loc (n - 1) with
  | Some l => fst l
  | None => _kw_endpos n
  end

pub let _kw_symbolstartpos _ = Parsing.error "unimplemented: $symbolstartpos"
pub let _kw_startofs _ = Parsing.error "unimplemented: $startofs"
pub let _kw_endofs _ = Parsing.error "unimplemented: $endofs"
pub let _kw_symbolstartofs _ = Parsing.error "unimplemented: $symbolstartofs"
pub let _kw_loc n = _kw_startpos n, _kw_endpos n
pub let _kw_sloc _ = Parsing.error "unimplemented: $sloc"
|} |> verbatim
[@@ocamlformat "disable"]

let state_lib = {|
let lexfun 
    {E_err, E_lex
    , ~error : Parsing.Error E_err
    , ~lex : Parsing.Lex E_lex Tok
    } ppos = 
  let (aux : Unit ->[E_err, E_lex] Tok) = 
  fn () => ~lex.token ppos 
  in aux ()

pub let shift 
        {E_err, E_lex
        , ~error : Parsing.Error E_err
        , ~lex : Parsing.Lex E_lex Tok
        } () = 
  let (aux : Unit ->[E_err, E_lex] Pair Tok (Pair Parsing.Pos Parsing.Pos)) = 
    (fn () => 
      let tok = lexfun () in
      let loc = ~lex.startPos (), ~lex.curPos () in
      (tok, loc))
  in aux ()


parameter ~loc
let locShift l = l :: ~loc

let locDummy xs = 
  match xs with
  | [] => (Parsing.dummyPos, Parsing.dummyPos)
  | (_, e) :: _ => (e, e)
  end

let locReduce 
    {E_err, E_lex
    , ~error : Parsing.Error E_err
    , ~lex : Parsing.Lex E_lex Tok
    } n =
  let (aux : Int ->[E_err, E_lex] List (Pair Parsing.Pos Parsing.Pos)) = 
    (fn (n : Int) =>
      if n == 0 then locDummy ~loc :: ~loc
      else
        (let rec skip (n : Int) xs =
           if n == 0 then xs
           else skip (n - 1)
               (List.tlErr {~onError = (fn () => Parsing.error "tl")}
                     xs) in
         let l = (fst (List.nthErr {~onError = (fn () => Parsing.error "nth")}
                        ~loc
                        (n - 1)),
                   snd (List.hdErr {~onError = (fn () => Parsing.error "hd")}
                       ~loc)) in
         l :: skip n ~loc))
  in aux n

parameter R_lex
parameter ~lex : Parsing.Lex R_lex Tok
parameter E_err
parameter ~error : Parsing.Error E_err
|} |> verbatim
[@@ocamlformat "disable"]

let error_handler = {|
Parsing.Error (effect x / _ => Left x)
  return x => Right x
|} |> verbatim
[@@ocamlformat "disable"]

module Make (S : Types.BackEndSettings) (G : Types.Grammar) (A : Types.Automaton) :
  Types.Code = struct
  open Automaton
  module PP = Printer.Fram (S)
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

  (** Checks whether this is the last shift before end-of-stream is (possibly) reached *)
  let is_eof_shift state sym =
    let closure = state.s_kernel @ state.s_closure in
    let group = List.find (shifts_group (Symbol.Term sym)) closure in
    let item = List.find (shifts_item (Symbol.Term sym)) group.g_items in
    List.length item.i_suffix = 1
    && TermSet.equal group.g_lookahead (TermSet.singleton Terminal.eof)
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

  let make_vars vars expr =
    let binding (name, valu) = PP.{ name; named_args = []; args = []; expr = valu; comment = None } in
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
      | Raw.KwStartpos -> Printf.sprintf "(_kw_startpos %d)" n
      | Raw.KwEndpos -> Printf.sprintf "(_kw_endpos %d)" n
      | Raw.KwSymbolstartpos -> Printf.sprintf "(_kw_symbolstartpos %d)" n
      | Raw.KwStartofs -> Printf.sprintf "(_kw_startofs %d)" n
      | Raw.KwEndofs -> Printf.sprintf "(_kw_endofs %d)" n
      | Raw.KwSymbolstartofs -> Printf.sprintf "(_kw_symbolstartofs %d)" n
      | Raw.KwLoc -> Printf.sprintf "(_kw_loc %d)" n
      | Raw.KwSloc -> Printf.sprintf "(_kw_sloc %d)" n
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
    let named_args = [PP.ExprLabeled "loc"] in
    let expr = make_semantic_action_code action in
    PP.{ name = semantic_action_id action id; named_args; args; expr; comment = None }
  ;;

  let make_semantic_actions actions =
    let bindings = IntMap.bindings actions |> List.map make_semantic_action in
    if IntMap.cardinal actions = 0
    then PP.StructVerbatim (verbatim "(* No actions *)")
    else PP.StructLet (PP.Public, PP.NonRecursive, bindings)
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
    PP.ExprCall (callee, tok @ valu @ args @ const)
  ;;

  let make_continuation state group idx =
    let sym = NTerm group.g_symbol in
    let name = cont_id group idx
    and args = [ PP.ExprId "t"; PP.ExprId "x" ]
    and named_args = [ PP.ExprLabeled "loc" ]
    and body = make_goto_call state sym in
    PP.{ name; named_args; args; expr = body; comment = None }
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
        let expr = PP.ExprGrouped (PP.ExprCall (name, args)) in
        sym, i, expr :: acc
      | [], _ -> assert false
    in
    let sym, _, acc = List.fold_right aux call (symbols, 0, []) in
    assert (sym = []);
    List.rev_append acc [ PP.ExprUnit ]
  ;;

  let make_semantic_action_call group = function
    | { i_action = None; _ } ->
      let args = make_args_ids group.g_prefix in
      assert (List.length args = 1);
      List.hd args
    | { i_action = Some a; _ } ->
      let action = IntMap.find a.ac_id A.automaton.a_actions in
      let name = semantic_action_id action a.ac_id |> Printf.sprintf "Actions.%s" in
      let args = make_semantic_action_args a.ac_args group.g_prefix in
      PP.ExprCall (name, args)
  ;;

  let make_action_shift state sym =
    let patterns =
      match symbol_has_value (Term sym) with
      | true -> [ term_name sym, Some "x" ]
      | false -> [ term_name sym, None ]
    in
    let shift = PP.ExprVerbatim [ verbatim "shift ()" ]
    and locs = PP.ExprVerbatim [ verbatim "locShift (snd t)" ]
    and comment = if S.comments then Some " Shift " else None in
    let expr =
      let expr = make_goto_call state (Term sym) in
      (* When, after shifting we can reach the end of stream, we should not look at the token that follows.
         Instead, let's reuse the token that we already have, because we shouldn't look at it anyway *)
      let expr = if is_eof_shift state sym then expr else make_var "t" shift expr in
      if S.locations then make_var "~loc" locs expr else expr
    in
    PP.{ patterns; cexpr = expr; ccomment = comment }
  ;;

  let make_action_reduce state lookahead i j =
    let group = List.nth (state.s_kernel @ state.s_closure) i in
    let n, item = List.length group.g_prefix, List.nth group.g_items j in
    let pattern sym =
      match symbol_has_value (Term sym) with
      | _ when sym = Terminal.eof -> "_", None
      | true -> term_name sym, Some "_"
      | false -> term_name sym, None
    in
    let call = make_semantic_action_call group item
    and patterns = TermSet.elements lookahead |> List.map pattern
    and comment = if S.comments then Some " Reduce " else None
    and locs = PP.ExprVerbatim [ Printf.sprintf "locReduce %d" n |> verbatim ] in
    let expr =
      let args = [ PP.ExprId "t"; PP.ExprId "x" ] in
      let expr = PP.ExprCall (cont_id group i, args) in
      let vars = if S.locations then [ "~loc", locs; "x", call ] else [ "x", call ] in
      make_vars vars expr
    in
    PP.{ patterns; cexpr = expr; ccomment = comment }
  ;;

  let make_action state lookahead = function
    | Shift ->
      TermSet.to_seq lookahead |> Seq.map (make_action_shift state) |> List.of_seq
    | Reduce (i, j) -> [ make_action_reduce state lookahead i j ]
  ;;

  let make_action_failure _ = (* argument is a state *)
    let failure = verbatim "Parsing.error \"\""
    (* TODO: expected terms in error message.
      List.fold_left (fun acc (t, _) -> TermSet.union t acc) TermSet.empty state.s_action
      |> TermSet.elements
      |> List.map (fun t -> term_name t |> Printf.sprintf "%S")
      |> String.concat "; "
      |> Printf.sprintf "fail t [ %s ]"
      |> verbatim
      *)
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
    and named_args = [ PP.ExprLabeled "loc" ]
    and cont = make_cont_ids (Fun.const true) state.s_kernel
    and body = make_state_body state
    and comment = if S.comments then Some (make_state_comment state) else None in
    PP.{ name; named_args; args = tok @ args @ cont; expr = body; comment }
  ;;

  let make_states states =
    if IntMap.cardinal states = 0
    then PP.StructVerbatim (verbatim "(* No states *)")
    else PP.StructLet (PP.Public, PP.Recursive, IntMap.bindings states |> List.map make_state)
  ;;

  let make_entry symbol id =
    let state = Printf.sprintf "States.%s" (state_id id)
    and args = [ PP.ExprUnit ]
    and cont = PP.ExprVerbatim [ verbatim "(fn _ x => x)" ] in
    let expr = PP.ExprCall (state, [ PP.ExprId "t"; cont ]) in
    let expr = make_var "t" (PP.ExprVerbatim [ verbatim "States.shift ()" ]) expr in
    let expr = make_var "~loc" (PP.ExprVerbatim [ verbatim "[]" ]) expr in
    let expr = PP.ExprHandle (
      { name = "~error"; named_args = []; args = [];
        expr = PP.ExprVerbatim [ error_handler ]; comment = None },
      expr) in
    let named_args = [ PP.ExprLabeled "lex" ] in
    let binding = PP.{ name = nterm_name symbol; named_args; args; expr; comment = None } in
    PP.StructLet (PP.Public, PP.NonRecursive, [ binding ])
  ;;

  let make_file { a_header; a_actions; a_states; a_starting } =
    let header = List.map (fun n -> PP.StructVerbatim n) a_header
    and actions = [ PP.StructVerbatim action_lib; make_semantic_actions a_actions ]
    and states = [ PP.StructVerbatim state_lib; make_states a_states ]
    and starting = List.map (fun (sym, id) -> make_entry sym id) a_starting in
    []
    @ [ PP.StructVerbatim prelude ]
    @ header
    @ [ PP.StructType (PP.Public, PP.NonRecursive, "Tok", make_token_type G.symbols)
      ; PP.StructModule (PP.Public, "Actions", actions)
      ; PP.StructModule (PP.Public, "States", states)
      ]
    @ starting
  ;;

  let write () = make_file A.automaton |> PP.pp_structures
end
