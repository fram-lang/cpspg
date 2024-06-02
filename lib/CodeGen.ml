module IntMap = Map.Make (Int)
module SymbolMap = Map.Make (Automaton.Symbol)

let action_lib =
  "  implicit `loc\n\
  \  implicit `error {E_err} : Parsing.Error E_err\n\n\
  \  pub let _kw_endpos _ =\n\
  \    match `loc with\n\
  \    | l :: _ => snd l\n\
  \    | [] => Parsing.dummyPos\n\
  \    end\n\n\
  \  pub let _kw_startpos (n : Int) =\n\
  \    match Utils.nth_opt `loc (n - 1) with\n\
  \    | Some l => fst l\n\
  \    | None => _kw_endpos n\n\
  \    end\n\n\
  \  pub let _kw_symbolstartpos _ = Parsing.error \"unimplemented: $symbolstartpos\"\n\
  \  pub let _kw_startofs _ = Parsing.error \"unimplemented: $startofs\"\n\
  \  pub let _kw_endofs _ = Parsing.error \"unimplemented: $endofs\"\n\
  \  pub let _kw_symbolstartofs _ = Parsing.error \"unimplemented: $symbolstartofs\"\n\
  \  pub let _kw_loc n = _kw_startpos n, _kw_endpos n\n\
  \  pub let _kw_sloc _ = Parsing.error \"unimplemented: $sloc\"
  \n"
;;

let state_lib =
 "  let lexfun {E_err, E_st, R_lex,\n\
 \               `error : Parsing.Error E_err,\n\
 \     	  `st : State2 E_st,\n\
 \     	  `lex : Parsing.Lex R_lex Tok} () = \n\
 \     let (aux : Unit ->[E_err, E_st|R_lex] Tok) = \n\
 \     fn () => `lex.token ()\n\
 \     in aux ()\n\n\
 \   let shift {E_err, E_st, R_lex,\n\
 \     	 `error : Parsing.Error E_err,\n\
 \     	 `st : State2 E_st,\n\
 \     	 `lex : Parsing.Lex R_lex Tok} () = \n\
 \     let (aux : Unit ->[E_err, E_st|R_lex] Pair Tok (Pair Parsing.Pos Parsing.Pos)) = \n\
 \     (fn () => \n\
 \         let sym = Utils.optionGet {`re = (fn () => Parsing.error \"option\")}\n\
 \     			      (getPeeked ()) in\n\
 \         let () = setPeeked None in\n\
 \         let () = setFallback (`lex.curPos ()) in\n\
 \         sym)\n\
 \     in aux ()\n\n\
 \   let lookahead {E_err, E_st, R_lex,\n\
 \     	     `error : Parsing.Error E_err,\n\
 \     	     `st : State2 E_st,\n\
 \     	     `lex : Parsing.Lex R_lex Tok} () = \n\
 \     let (aux : Unit ->[E_err, E_st|R_lex] Tok) = \n\
 \     (fn () => \n\
 \         match getPeeked () with\n\
 \         | Some (tok, _) => tok\n\
 \         | None =>\n\
 \           let tok = lexfun () in\n\
 \           let loc = `lex.startPos (), `lex.curPos () in\n\
 \           let () = setPeeked (Some (tok, loc)) in\n\
 \           tok\n\
 \         end)\n\
 \     in aux ()\n\n\
 \   implicit `loc\n\
 \   let loc_shift l = l :: `loc\n\n\
 \   let loc_reduce {E_err, E_st, R_lex,\n\
 \     	      `error : Parsing.Error E_err,\n\
 \     	      `st : State2 E_st,\n\
 \     	      `lex : Parsing.Lex R_lex Tok} n =\n\
 \     let (aux : Int ->[E_err, E_st|R_lex] List (Pair Parsing.Pos Parsing.Pos)) = \n\
 \     (fn (n : Int) =>\n\
 \         if n == 0 then (getFallback (), getFallback ()) :: `loc\n\
 \         else\n\
 \           (let rec skip (n : Int) xs =\n\
 \     	 if n == 0 then xs\n\
 \     	 else skip (n - 1)\n\
 \     		   (Utils.tl {`re = (fn () => Parsing.error \"tl\")}\n\
 \     			     xs) in\n\
 \            let l = (fst (Utils.nth {`re = (fn () => Parsing.error \"nth\")}\n\
 \     			      `loc\n\
 \     			      (n - 1)),\n\
 \     		snd (Utils.hd {`re = (fn () => Parsing.error \"hd\")}\n\
 \     			      `loc)) in\n\
 \            l :: skip n `loc))\n\
 \     in aux n\n\n\
 \   implicit `lex {R_lex} : Parsing.Lex R_lex Tok\n\
 \   implicit `st {E_st} : State2 E_st\n\
 \   implicit `error {E_err} : Parsing.Error E_err\n\
 \n"
;;

let iteri2 f xs ys =
  let f i x y =
    f i x y;
    i + 1
  in
  List.fold_left2 f 0 xs ys |> ignore
;;

module Make (S : Types.Settings) (G : Types.Grammar) (A : Types.Automaton) : Types.Code =
struct
  open Automaton
  module D = Graphviz.Make (G)

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

  let indent s i =
    String.trim s
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> String.concat ("\n" ^ i)
  ;;

  let letrec ?(pre = "rec let") ?(pre' = "let") ?(post = "") ?(post' = " end in") f xs =
    let rec loop i = function
      | [] -> ()
      | x :: xs ->
        f i x (if i = 0 then pre else pre') (if xs = [] then post' else post);
        loop (i + 1) xs
    in
    loop 0 xs
  ;;

  let write_line_directive f (loc, _) =
    Format.fprintf
      f
      "\n# %d \"%s\"\n%s"
      loc.Lexing.pos_lnum
      loc.Lexing.pos_fname
      (String.make (loc.pos_cnum - loc.pos_bol) ' ')
  ;;

  let write_string f { loc; data } =
    if S.line_directives
    then Format.fprintf f "%t%s" (fun f -> write_line_directive f loc) data
    else Format.fprintf f "%s" (String.trim data)
  ;;

  let write_arg_id f symbol idx =
    if S.readable_ids
    then Format.fprintf f "a%d_%s" idx (symbol_name symbol)
    else Format.fprintf f "a%d" idx
  ;;

  (* Continuations are prefixed with underscore because
     precedence declarations could make them unused
     (see unary minus in `calc/ParserPres.mly`) *)
  let write_cont_id f group idx =
    match S.readable_ids, group.g_starting with
    | false, _ -> Format.fprintf f "_c%d" idx
    | true, false -> Format.fprintf f "_c%d_%s" idx (nterm_name group.g_symbol)
    | true, true -> Format.fprintf f "_c%d_%s_starting" idx (nterm_name group.g_symbol)
  ;;

  let write_semantic_action_id f action idx =
    if S.readable_ids
    then Format.fprintf f "a%d_%s" idx (nterm_name action.sa_symbol)
    else Format.fprintf f "a%d" idx
  ;;

  let write_state_id f idx =
    if S.readable_ids then Format.fprintf f "state_%d" idx else Format.fprintf f "s%d" idx
  ;;

  let write_cont_ids f p groups =
    let iter i g = if p g then Format.fprintf f " %t" (fun f -> write_cont_id f g i) in
    List.iteri iter groups
  ;;

  let write_arg_ids f symbols =
    let iter i s =
      if symbol_has_value s then Format.fprintf f " %t" (fun f -> write_arg_id f s i)
    in
    List.iteri iter symbols
  ;;

  let write_term_pattern f bind t =
    if symbol_has_value (Term t)
    then Format.fprintf f "%s %s" (term_name t) (if bind then "x" else "_")
    else Format.fprintf f "%s" (term_name t)
  ;;

  (* This function is now obsolete because DBL has no disjunctions of patterns. *)
  let _write_term_patterns f ts =
    let f sym = Format.fprintf f "| %t " (fun f -> write_term_pattern f false sym) in
    TermSet.iter f ts
  ;;

  let write_goto_call f state sym =
    let closure = state.s_kernel @ state.s_closure in
    write_state_id f (SymbolMap.find sym state.s_goto);
    if symbol_has_value sym then Format.fprintf f " x";
    write_arg_ids f (List.find (shifts_group sym) closure).g_prefix;
    write_cont_ids f (shifts_group sym) (state.s_kernel @ state.s_closure)
  ;;

  let write_cont_definition f state group idx =
    let sym = NTerm group.g_symbol in
    Format.fprintf
      f
      "%t%s x = %t"
      (fun f -> write_cont_id f group idx)
      (if S.locations then " {`loc}" else "") 
      (fun f -> write_goto_call f state sym)
  ;;

  let write_semantic_action_call f group = function
    | { i_action = -1; _ } ->
      assert (List.length group.g_prefix = 1);
      write_arg_ids f group.g_prefix
    | { i_action; _ } ->
      let action = IntMap.find i_action A.automaton.a_actions in
      Format.fprintf
        f
        " Actions.%t%t ()"
        (fun f -> write_semantic_action_id f action i_action)
        (fun f -> write_arg_ids f group.g_prefix)
  ;;

  let write_action_shift f state sym =
    let write_loc_update f = Format.fprintf f " in\n      let `loc = loc_shift _l" in
    if S.comments then Format.fprintf f "    (* Shift *)\n";
    Format.fprintf
      f
      "    | %t =>\n      let (_, _l) = shift ()%t in\n      %t\n"
      (fun f -> write_term_pattern f true sym)
      (fun f -> if S.locations then write_loc_update f)
      (fun f -> write_goto_call f state (Term sym))
  ;;

  let write_action_reduce f state lookahead i j =
    let write_loc_update f n =
      Format.fprintf f "\n      in let `loc = loc_reduce %d" n
    in
    if S.comments then Format.fprintf f "    (* Reduce *)\n";
    let group = List.nth (state.s_kernel @ state.s_closure) i in
    let n = List.length group.g_prefix
    and item = List.nth group.g_items j in
    TermSet.iter (fun sym ->
		    Format.fprintf
		      f
		      "    | %t =>\n      let x =%t%t in\n      %t x\n"
		      (fun f -> write_term_pattern f false sym)
		      (fun f -> write_semantic_action_call f group item)
		      (fun f -> if S.locations then write_loc_update f n)
		      (fun f -> write_cont_id f group i))
		 lookahead
  ;;

  let write_action f state lookahead = function
    | Shift -> TermSet.iter (write_action_shift f state) lookahead
    | Reduce (i, j) -> write_action_reduce f state lookahead i j
  ;;

  let write_actions f state =
    Format.fprintf f "    match lookahead () with\n";
    List.iter (fun (l, m) -> write_action f state l m) state.s_action;
    Format.fprintf f "    | _ => Parsing.error \"\"\n    end\n"
  ;;

  let write_actions_starting f state =
    if S.comments then Format.fprintf f "    (* Reduce *)\n";
    let group = List.hd state.s_kernel in
    let item = List.nth group.g_items 0 in
    Format.fprintf
      f
      "    let x =%t in\n    %t x\n"
      (fun f -> write_semantic_action_call f group item)
      (fun f -> write_cont_id f group 0)
  ;;

  let write_term_cons f = function
    | { ti_name; ti_ty = None; _ } ->
      Format.fprintf f "  | %t\n" (fun f -> write_string f ti_name)
    | { ti_name; ti_ty = Some ty; _ } ->
      Format.fprintf
        f
        "  | %t of (%t)\n"
        (fun f -> write_string f ti_name)
        (fun f -> write_string f ty)
  ;;

  let write_term_type f symbols =
    let get_info = function
      | NTerm _ -> None
      | Term t -> Some (G.term t)
    and cmp a b = String.compare b.ti_name.data a.ti_name.data in
    let infos = List.filter_map get_info symbols in
    let infos = List.fast_sort cmp infos in
    Format.fprintf f "pub data Tok =\n";
    List.iter (write_term_cons f) infos;
    Format.fprintf f "\n"
  ;;

  let write_semantic_action_code f action =
    let n = List.length action.sa_args
    and code = action.sa_code in
    let write_part f l r =
      let len = r.Lexing.pos_cnum - l.Lexing.pos_cnum
      and ofs = l.pos_cnum - (fst action.sa_code.loc).pos_cnum in
      write_string f { data = String.sub (fst code.data) ofs len; loc = l, r }
    and get_impl = function
      | Ast.KwArg i ->
        (match List.nth_opt action.sa_args (i - 1) with
         | Some (Some a) -> a
         | Some None -> Printf.sprintf "_arg%d" i
         | None -> "()")
      | Ast.KwStartpos -> Printf.sprintf "_kw_startpos %d" n
      | Ast.KwEndpos -> Printf.sprintf "_kw_endpos %d" n
      | Ast.KwSymbolstartpos -> Printf.sprintf "_kw_symbolstartpos %d" n
      | Ast.KwStartofs -> Printf.sprintf "_kw_startofs %d" n
      | Ast.KwEndofs -> Printf.sprintf "_kw_endofs %d" n
      | Ast.KwSymbolstartofs -> Printf.sprintf "_kw_symbolstartofs %d" n
      | Ast.KwLoc -> Printf.sprintf "_kw_loc %d" n
      | Ast.KwSloc -> Printf.sprintf "_kw_sloc %d" n
    in
    let rec loop pos = function
      | [] -> write_part f pos (snd code.loc)
      | (kw, loc) :: kws ->
        write_part f pos (fst loc);
        let impl = get_impl kw in
        Format.fprintf f "(%s) " impl;
        loop (snd loc) kws
    in
    loop (fst action.sa_code.loc) (snd action.sa_code.data |> List.rev)
  ;;

  let write_semantic_action f id action =
    let item = List.nth (G.group action.sa_symbol).g_items action.sa_index in
    let iter i s = function
      | _ when symbol_has_value s = false -> ()
      | Some a -> Format.fprintf f " %s" a
      | None -> Format.fprintf f " _arg%d" (List.length action.sa_args - i)
    in
    write_semantic_action_id f action id;
    iteri2 iter (List.rev item.i_suffix) (List.rev action.sa_args);
    Format.fprintf f " () = %t" (fun f -> write_semantic_action_code f action)
  ;;

  let write_state_comment f state =
    let ci = Format.asprintf "%a" D.fmt_state state
    and cs = Format.asprintf "%a" D.fmt_state_shifts state
    and ca = Format.asprintf "%a" D.fmt_state_actions state in
    Format.fprintf
      f
      "  (* ITEMS:\n       %s\n     GOTO:\n       %s\n     ACTION:\n       %s *)\n"
      (indent ci "       ")
      (indent cs "       ")
      (indent ca "       ")
  ;;

  let write_state_sig f id state =
    Format.fprintf
      f
      "%t%s%t%t =\n"
      (fun f -> write_state_id f id)
      (if S.locations then " {`loc}" else "")
      (fun f -> write_arg_ids f (List.hd state.s_kernel).g_prefix)
      (fun f -> write_cont_ids f (fun _ -> true) state.s_kernel)
  ;;

  let write_state_body f state =
    let kn = List.length state.s_kernel in
    let gen_state_cont_def i group pre post =
      let fc f = write_cont_definition f state group (i + kn) in
      Format.fprintf f "    %s %t%s\n" pre fc post
    in
    letrec gen_state_cont_def state.s_closure;
    let group = List.hd state.s_kernel in
    if group.g_starting && (List.hd group.g_items).i_suffix = []
    then write_actions_starting f state
    else write_actions f state
  ;;

  let write_state f id state =
    write_state_sig f id state;
    write_state_body f state
  ;;

  let write_entry f symbol id =
    Format.fprintf
      f
      "pub let %s {`lex} () =\n\
      \  handle `error = Parsing.Error (effect x / _ => Utils.Left x)\n\
      \    return x => Utils.Right x in\n\
      \  handle `st = State2\n\
      \    { setPeeked = effect p / r => fn _ f => r () p f\n\
      \    , getPeeked = effect () / r => fn p f => r p p f\n\
      \    , setFallback = effect f / r => fn p _ => r () p f\n\
      \    , getFallback = effect () / r => fn p f => r f p f }\n\
      \    return x => fn _ _ => x\n\
      \    finally f => f None Parsing.dummyPos in\n\
      \  States.%t%s (fn x => x)\n\
      \n"
      (nterm_name symbol)
      (fun f -> write_state_id f id)
      (if S.locations then " {`loc = []}" else "")
  ;;

  let write f =
    let write_semantic_action f id a =
      Format.fprintf f "  pub let %t\n" (fun f -> write_semantic_action f id a)
    and write_state f _ (id, s) pre post =
      if S.comments then write_state_comment f s;
      Format.fprintf f "  %s %t%s" pre (fun f -> write_state f id s) post
    and write_entry f (nt, s) = write_entry f nt s
    and state_letrec = letrec ~pre:"pub rec let" ~post:"\n" ~post':"end  \n" in
    (* -unused-rec-flag due continuations always being mutually recursive, while often they don't need to *)
    (* FIXME: should we include -redunant-{case, subpat}? They trigger warnings
       in grammars with unresolved conflicts, but maybe it's a good thing? *)
    Format.fprintf
      f
      "import Parsing\n\
       import Utils\n\
       implicit `error {E_err} : Parsing.Error E_err\n\
       %t\n\n\
       %tdata State2 (effect E) = State2 of\n\
       \  { setPeeked : Option (Pair Tok (Pair Parsing.Pos Parsing.Pos)) ->[E] Unit\n\
       \  , setFallback : Parsing.Pos ->[E] Unit\n\
       \  , getPeeked : Unit ->[E] Option (Pair Tok (Pair Parsing.Pos Parsing.Pos))\n\
       \  , getFallback : Unit ->[E] Parsing.Pos }\n\n\
       method setPeeked {E, self = State2 {setPeeked} : State2 E} = setPeeked\n\
       method getPeeked {E, self = State2 {getPeeked} : State2 E} = getPeeked\n\
       method setFallback {E, self = State2 {setFallback} : State2 E} = setFallback\n\
       method getFallback {E, self = State2 {getFallback} : State2 E} = getFallback\n\n\
       let setPeeked {E, `st : State2 E} p = `st.setPeeked p\n\
       let getPeeked {E, `st : State2 E} () = `st.getPeeked ()\n\
       let setFallback {E, `st : State2 E} f = `st.setFallback f\n\
       let getFallback {E, `st : State2 E} () = `st.getFallback ()\n\n\
       module Actions\n\
       %s%tend\n\n\
       module States\n\
       %s%tend\n\n\
       %t" 
      (fun f -> write_string f A.automaton.a_header)
      (fun f -> write_term_type f G.symbols)
      action_lib
      (fun f -> IntMap.iter (write_semantic_action f) A.automaton.a_actions) (* FIXME: rec blocks are not indented *)
      state_lib
      (fun f -> IntMap.bindings A.automaton.a_states |> state_letrec (write_state f)) (* FIXME: rec blocks are not indented *)
      (fun f -> List.iter (write_entry f) A.automaton.a_starting)
  ;;
end
