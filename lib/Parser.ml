[@@@warning "-unused-rec-flag"]
[@@@warning "-redundant-case"]
[@@@warning "-redundant-subpat"]

open Raw

let plus, star, qmark =
  let span = Lexing.dummy_pos, Lexing.dummy_pos in
  let sym data = { span; data = NTerm data } in
  sym "nonempty_list", sym "list", sym "option"
;;

type token =
  | TYPE of (string)
  | TID of (string)
  | STAR
  | SEMI
  | RPAREN
  | QMARK
  | PLUS
  | LPAREN
  | ID of (string)
  | EQ
  | EOF
  | DWHEN
  | DTYPE
  | DTOKEN
  | DSTART
  | DSEP
  | DRIGHT
  | DPREC
  | DNONASSOC
  | DLEFT
  | DINLINE
  | DCODE of (string)
  | COMMA
  | COLON
  | CODE of (Raw.code)
  | BAR

module Actions = struct
  let _kw_endpos ~loc _ =
    match loc with
    | l :: _ -> snd l
    | [] -> Lexing.dummy_pos
  ;;

  let _kw_startpos ~loc n =
    match List.nth_opt loc (n - 1) with
    | Some l -> fst l
    | None -> _kw_endpos ~loc n
  ;;

  let _kw_symbolstartpos ~loc:_ _ = failwith "unimplemented: $symbolstartpos"
  let _kw_startofs ~loc:_ _ = failwith "unimplemented: $startofs"
  let _kw_endofs ~loc:_ _ = failwith "unimplemented: $endofs"
  let _kw_symbolstartofs ~loc:_ _ = failwith "unimplemented: $symbolstartofs"
  let _kw_loc ~loc n = _kw_startpos ~loc n, _kw_endpos ~loc n
  let _kw_sloc ~loc:_ _ = failwith "unimplemented: $sloc"
  let a0 ~loc:_loc data () = ({ span =(_kw_loc ~loc:_loc 1) ; data })
  let a1 ~loc:_loc data () = (DeclCode data)
  let a2 ~loc:_loc _arg1 () = ((_arg1) )
  let a3 ~loc:_loc () = (None)
  let a4 ~loc:_loc x () = (Some x)
  let a5 ~loc:_loc _arg1 () = ((_arg1) )
  let a6 ~loc:_loc () = ([])
  let a7 ~loc:_loc xs x () = (x :: xs)
  let a8 ~loc:_loc xs tp _arg1 () = (DeclToken (tp, xs))
  let a9 ~loc:_loc _arg1 () = ((_arg1) )
  let a10 ~loc:_loc xs tp _arg1 () = (DeclStart (tp, xs))
  let a11 ~loc:_loc x () = ({ span =(_kw_loc ~loc:_loc 1) ; data = NTerm x })
  let a12 ~loc:_loc x () = ({ span =(_kw_loc ~loc:_loc 1) ; data = Term x })
  let a13 ~loc:_loc xs tp _arg1 () = (DeclType  (tp, xs))
  let a14 ~loc:_loc x () = (x)
  let a15 ~loc:_loc x () = (x)
  let a16 ~loc:_loc xs _arg1 () = (DeclLeft xs)
  let a17 ~loc:_loc xs _arg1 () = (DeclRight xs)
  let a18 ~loc:_loc xs _arg1 () = (DeclNonassoc xs)
  let a19 ~loc:_loc () = (false)
  let a20 ~loc:_loc _arg1 () = (true)
  let a21 ~loc:_loc x () = (x)
  let a22 ~loc:_loc x () = ([ x ])
  let a23 ~loc:_loc xs _arg2 x () = (x :: xs)
  let a24 ~loc:_loc () = ([])
  let a25 ~loc:_loc x () = (x)
  let a26 ~loc:_loc xs () = (xs)
  let a27 ~loc:_loc _arg3 params _arg1 () = (params)
  let a28 ~loc:_loc _arg2 x () = (x)
  let a29 ~loc:_loc () = (None)
  let a30 ~loc:_loc x () = (Some x)
  let a31 ~loc:_loc _arg1 () = (plus)
  let a32 ~loc:_loc _arg1 () = (star)
  let a33 ~loc:_loc _arg1 () = (qmark)
  let a34 ~loc:_loc a_symbol a_actual () = ({ a_symbol; a_args = [ Arg a_actual ] })
  let a35 ~loc:_loc x () = (Arg x)
  let a36 ~loc:_loc _arg1 () = ((_arg1) )
  let a37 ~loc:_loc a_action a_prod () = (ArgInline { a_prod; a_action })
  let a38 ~loc:_loc _arg3 args _arg1 () = (args)
  let a39 ~loc:_loc a_args a_symbol () = ({ a_symbol; a_args })
  let a40 ~loc:_loc _arg3 p_actual p_id () = ({ p_id; p_actual })
  let a41 ~loc:_loc x _arg1 () = (x)
  let a42 ~loc:_loc a_code rhs _arg3 lhs _arg1 () = ({ a_cond = Some ((lhs, rhs)); a_code })
  let a43 ~loc:_loc x () = ([ x ])
  let a44 ~loc:_loc xs x () = (x :: xs)
  let a45 ~loc:_loc xs () = (xs)
  let a46 ~loc:_loc a_code xs () = (xs @ [{ a_cond = None; a_code }])
  let a47 ~loc:_loc p_actions p_prec p_prod () = ({ p_prod; p_prec; p_actions })
  let a48 ~loc:_loc _arg7 r_prods _arg5 _arg4 r_params r_id r_inline () = ({ r_id; r_inline; r_params; r_prods })
  let a49 ~loc:_loc _arg4 r_rules _arg2 r_decls () = ({ r_decls; r_rules })
end

module States = struct
  let lexfun = ref (fun _ -> assert false)
  let lexbuf = ref (Lexing.from_string String.empty)
  let peeked = ref None
  let lexbuf_fallback_p = ref Lexing.dummy_pos
  let error_token = ref None
  let expected_tokens = ref []

  let setup lf lb =
    lexfun := lf;
    lexbuf := lb;
    peeked := None;
    lexbuf_fallback_p := !lexbuf.lex_curr_p;
    error_token := None;
    expected_tokens := []
  ;;

  let shift () =
    let sym = Option.get !peeked in
    peeked := None;
    lexbuf_fallback_p := !lexbuf.lex_curr_p;
    sym
  ;;

  let peek () =
    match !peeked with
    | Some p -> p
    | None ->
      let tok = !lexfun !lexbuf
      and loc = !lexbuf.lex_start_p, !lexbuf.lex_curr_p in
      peeked := Some (tok, loc);
      tok, loc
  ;;

  let lookahead () = fst (peek ())

  let fail expected =
    let token, _ = peek () in
    error_token := Some token;
    expected_tokens := expected;
    raise Parsing.Parse_error
  ;;

  let loc_shift ~loc l = l :: loc

  let loc_reduce ~loc = function
    | 0 -> (!lexbuf_fallback_p, !lexbuf_fallback_p) :: loc
    | n ->
      let rec skip n xs = if n = 0 then xs else skip (n - 1) (List.tl xs) in
      let l = fst (List.nth loc (n - 1)), snd (List.hd loc) in
      l :: skip n loc
  ;;

  (* ITEMS:
       grammar' → . list DSEP list EOF
       decl → . DTOKEN option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTYPE TYPE list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DLEFT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DRIGHT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DNONASSOC list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DCODE 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . decl list 		/ DSEP
       list → . 		/ DSEP
     GOTO:
       DCODE -> 1
       DTOKEN -> 2
       DTYPE -> 8
       DSTART -> 15
       DLEFT -> 20
       DRIGHT -> 26
       DNONASSOC -> 28
       decl -> 30
       list -> 32
     ACTION:
       DSEP -> reduce 2 1
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift *)
  let rec state_0 ~loc _c0_grammar_starting =
    let rec _c1_decl ~loc x = state_30 ~loc x _c2_list
    and _c2_list ~loc x = state_32 ~loc x _c0_grammar_starting in
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    (* Shift *)
    | DCODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_1 ~loc x _c1_decl
    (* Shift *)
    | DTOKEN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_2 ~loc _c1_decl
    (* Shift *)
    | DTYPE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_8 ~loc _c1_decl
    (* Shift *)
    | DSTART ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_15 ~loc _c1_decl
    (* Shift *)
    | DLEFT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_20 ~loc _c1_decl
    (* Shift *)
    | DRIGHT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_26 ~loc _c1_decl
    (* Shift *)
    | DNONASSOC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_28 ~loc _c1_decl
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DCODE . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_1 ~loc a0_DCODE _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a1 ~loc (Actions.a0 ~loc a0_DCODE ()) () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN . option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . TYPE 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 3
       option -> 4
     ACTION:
       TYPE -> shift
       TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_2 ~loc _c0_decl =
    let rec _c1_option ~loc x = state_4 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_option
    (* Reduce *)
    | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a3 ~loc () in
      _c1_option ~loc x
    | _ -> fail [ "TID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       option → TYPE . 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       ID TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_3 ~loc a0_TYPE _c0_option =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a4 ~loc (Actions.a2 ~loc (Actions.a0 ~loc a0_TYPE ()) ()) () in
      _c0_option ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN option . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . TID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 5
       list -> 7
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_4 ~loc a0_option _c0_decl =
    let rec _c1_list ~loc x = state_7 ~loc x a0_option _c0_decl in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_5 ~loc x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → TID . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . TID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TID -> 5
       list -> 6
     ACTION:
       TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_5 ~loc a0_TID _c0_list =
    let rec _c1_list ~loc x = state_6 ~loc x a0_TID _c0_list in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_5 ~loc x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → TID list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_6 ~loc a0_list a1_TID _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list (Actions.a5 ~loc (Actions.a0 ~loc a1_TID ()) ()) () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTOKEN option list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_7 ~loc a0_list a1_option _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a8 ~loc a0_list a1_option () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTYPE . TYPE list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 9
     ACTION:
       TYPE -> shift *)
  and state_8 ~loc _c0_decl =
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_9 ~loc x _c0_decl
    | _ -> fail [ "TYPE" ]

  (* ITEMS:
       decl → DTYPE TYPE . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 12
       list -> 14
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_9 ~loc a0_TYPE _c0_decl =
    let rec _c1_symbol ~loc x = state_12 ~loc x _c2_list
    and _c2_list ~loc x = state_14 ~loc x a0_TYPE _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       symbol → ID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DWHEN, DPREC, COMMA, EQ, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN DPREC COMMA EQ PLUS QMARK SEMI STAR LPAREN RPAREN -> reduce 0 0 *)
  and state_10 ~loc a0_ID _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DWHEN | DPREC | COMMA | EQ | PLUS | QMARK | SEMI | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a11 ~loc a0_ID () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DWHEN"; "DPREC"; "COMMA"; "EQ"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       symbol → TID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DWHEN, DPREC, COMMA, EQ, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN DPREC COMMA EQ PLUS QMARK SEMI STAR LPAREN RPAREN -> reduce 0 0 *)
  and state_11 ~loc a0_TID _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DWHEN | DPREC | COMMA | EQ | PLUS | QMARK | SEMI | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a12 ~loc a0_TID () in
      _c0_symbol ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DWHEN"; "DPREC"; "COMMA"; "EQ"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       list → symbol . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       symbol → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . symbol list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 12
       list -> 13
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_12 ~loc a0_symbol _c0_list =
    let rec _c1_symbol ~loc x = state_12 ~loc x _c2_list
    and _c2_list ~loc x = state_13 ~loc x a0_symbol _c0_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → symbol list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_13 ~loc a0_list a1_symbol _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_symbol () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DTYPE TYPE list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_14 ~loc a0_list a1_TYPE _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a13 ~loc a0_list (Actions.a2 ~loc (Actions.a0 ~loc a1_TYPE ()) ()) () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART . option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . TYPE 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       option → . 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       TYPE -> 3
       option -> 16
     ACTION:
       TYPE -> shift
       ID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_15 ~loc _c0_decl =
    let rec _c1_option ~loc x = state_16 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_3 ~loc x _c1_option
    (* Reduce *)
    | ID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a3 ~loc () in
      _c1_option ~loc x
    | _ -> fail [ "ID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART option . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . ID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 17
       list -> 19
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_16 ~loc a0_option _c0_decl =
    let rec _c1_list ~loc x = state_19 ~loc x a0_option _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_17 ~loc x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → ID . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . ID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 17
       list -> 18
     ACTION:
       ID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_17 ~loc a0_ID _c0_list =
    let rec _c1_list ~loc x = state_18 ~loc x a0_ID _c0_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_17 ~loc x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → ID list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_18 ~loc a0_list a1_ID _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list (Actions.a9 ~loc (Actions.a0 ~loc a1_ID ()) ()) () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DSTART option list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_19 ~loc a0_list a1_option _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a10 ~loc a0_list a1_option () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ident → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ident → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . ident list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 21
       TID -> 22
       ident -> 23
       list -> 25
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_20 ~loc _c0_decl =
    let rec _c1_ident ~loc x = state_23 ~loc x _c2_list
    and _c2_list ~loc x = state_25 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_21 ~loc x _c1_ident
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_22 ~loc x _c1_ident
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       ident → ID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DWHEN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN -> reduce 0 0 *)
  and state_21 ~loc a0_ID _c0_ident =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DWHEN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a14 ~loc (Actions.a0 ~loc a0_ID ()) () in
      _c0_ident ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DWHEN" ]

  (* ITEMS:
       ident → TID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DWHEN
     GOTO:
       
     ACTION:
       ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN -> reduce 0 0 *)
  and state_22 ~loc a0_TID _c0_ident =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DWHEN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a15 ~loc (Actions.a0 ~loc a0_TID ()) () in
      _c0_ident ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DWHEN" ]

  (* ITEMS:
       list → ident . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ident → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ident → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . ident list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 21
       TID -> 22
       ident -> 23
       list -> 24
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_23 ~loc a0_ident _c0_list =
    let rec _c1_ident ~loc x = state_23 ~loc x _c2_list
    and _c2_list ~loc x = state_24 ~loc x a0_ident _c0_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_21 ~loc x _c1_ident
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_22 ~loc x _c1_ident
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → ident list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_24 ~loc a0_list a1_ident _c0_list =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_ident () in
      _c0_list ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DLEFT list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_25 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a16 ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ident → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ident → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . ident list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 21
       TID -> 22
       ident -> 23
       list -> 27
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_26 ~loc _c0_decl =
    let rec _c1_ident ~loc x = state_23 ~loc x _c2_list
    and _c2_list ~loc x = state_27 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_21 ~loc x _c1_ident
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_22 ~loc x _c1_ident
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DRIGHT list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_27 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a17 ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ident → . ID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       ident → . TID 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . ident list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       ID -> 21
       TID -> 22
       ident -> 23
       list -> 29
     ACTION:
       ID TID -> shift
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1 *)
  and state_28 ~loc _c0_decl =
    let rec _c1_ident ~loc x = state_23 ~loc x _c2_list
    and _c2_list ~loc x = state_29 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_21 ~loc x _c1_ident
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_22 ~loc x _c1_ident
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    | _ -> fail [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       decl → DNONASSOC list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
     GOTO:
       
     ACTION:
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_29 ~loc a0_list _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a18 ~loc a0_list () () in
      _c0_decl ~loc x
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → decl . list 		/ DSEP
       decl → . DTOKEN option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DSTART option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DTYPE TYPE list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DLEFT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DRIGHT list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DNONASSOC list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       decl → . DCODE 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
       list → . decl list 		/ DSEP
       list → . 		/ DSEP
     GOTO:
       DCODE -> 1
       DTOKEN -> 2
       DTYPE -> 8
       DSTART -> 15
       DLEFT -> 20
       DRIGHT -> 26
       DNONASSOC -> 28
       decl -> 30
       list -> 31
     ACTION:
       DSEP -> reduce 2 1
       DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift *)
  and state_30 ~loc a0_decl _c0_list =
    let rec _c1_decl ~loc x = state_30 ~loc x _c2_list
    and _c2_list ~loc x = state_31 ~loc x a0_decl _c0_list in
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c2_list ~loc x
    (* Shift *)
    | DCODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_1 ~loc x _c1_decl
    (* Shift *)
    | DTOKEN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_2 ~loc _c1_decl
    (* Shift *)
    | DTYPE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_8 ~loc _c1_decl
    (* Shift *)
    | DSTART ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_15 ~loc _c1_decl
    (* Shift *)
    | DLEFT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_20 ~loc _c1_decl
    (* Shift *)
    | DRIGHT ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_26 ~loc _c1_decl
    (* Shift *)
    | DNONASSOC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_28 ~loc _c1_decl
    | _ -> fail [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (* ITEMS:
       list → decl list . 		/ DSEP
     GOTO:
       
     ACTION:
       DSEP -> reduce 0 0 *)
  and state_31 ~loc a0_list a1_decl _c0_list =
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_decl () in
      _c0_list ~loc x
    | _ -> fail [ "DSEP" ]

  (* ITEMS:
       grammar' → list . DSEP list EOF
     GOTO:
       DSEP -> 33
     ACTION:
       DSEP -> shift *)
  and state_32 ~loc a0_list _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | DSEP ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_33 ~loc a0_list _c0_grammar_starting
    | _ -> fail [ "DSEP" ]

  (* ITEMS:
       grammar' → list DSEP . list EOF
       rule → . boption ID loption COLON option separated_nonempty_list list 		/ ID, DINLINE, EOF
       boption → . DINLINE 		/ ID
       boption → . 		/ ID
       list → . rule list 		/ EOF
       list → . 		/ EOF
     GOTO:
       DINLINE -> 34
       rule -> 35
       boption -> 36
       list -> 98
     ACTION:
       ID -> reduce 2 1
       DINLINE -> shift
       EOF -> reduce 3 1 *)
  and state_33 ~loc a1_list _c0_grammar_starting =
    let rec _c1_rule ~loc x = state_35 ~loc x _c3_list
    and _c2_boption ~loc x = state_36 ~loc x _c1_rule
    and _c3_list ~loc x = state_98 ~loc x a1_list _c0_grammar_starting in
    match lookahead () with
    (* Reduce *)
    | ID _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a19 ~loc () in
      _c2_boption ~loc x
    (* Shift *)
    | DINLINE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_34 ~loc _c2_boption
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c3_list ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "EOF" ]

  (* ITEMS:
       boption → DINLINE . 		/ ID
     GOTO:
       
     ACTION:
       ID -> reduce 0 0 *)
  and state_34 ~loc _c0_boption =
    match lookahead () with
    (* Reduce *)
    | ID _ ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a20 ~loc () () in
      _c0_boption ~loc x
    | _ -> fail [ "ID" ]

  (* ITEMS:
       list → rule . list 		/ EOF
       rule → . boption ID loption COLON option separated_nonempty_list list 		/ ID, DINLINE, EOF
       boption → . DINLINE 		/ ID
       boption → . 		/ ID
       list → . rule list 		/ EOF
       list → . 		/ EOF
     GOTO:
       DINLINE -> 34
       rule -> 35
       boption -> 36
       list -> 97
     ACTION:
       ID -> reduce 2 1
       DINLINE -> shift
       EOF -> reduce 3 1 *)
  and state_35 ~loc a0_rule _c0_list =
    let rec _c1_rule ~loc x = state_35 ~loc x _c3_list
    and _c2_boption ~loc x = state_36 ~loc x _c1_rule
    and _c3_list ~loc x = state_97 ~loc x a0_rule _c0_list in
    match lookahead () with
    (* Reduce *)
    | ID _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a19 ~loc () in
      _c2_boption ~loc x
    (* Shift *)
    | DINLINE ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_34 ~loc _c2_boption
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c3_list ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "EOF" ]

  (* ITEMS:
       rule → boption . ID loption COLON option separated_nonempty_list list 		/ ID, DINLINE, EOF
     GOTO:
       ID -> 37
     ACTION:
       ID -> shift *)
  and state_36 ~loc a0_boption _c0_rule =
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_37 ~loc x a0_boption _c0_rule
    | _ -> fail [ "ID" ]

  (* ITEMS:
       rule → boption ID . loption COLON option separated_nonempty_list list 		/ ID, DINLINE, EOF
       parameters → . LPAREN loption RPAREN 		/ COLON
       loption → . parameters 		/ COLON
       loption → . 		/ COLON
     GOTO:
       LPAREN -> 38
       parameters -> 45
       loption -> 46
     ACTION:
       LPAREN -> shift
       COLON -> reduce 2 1 *)
  and state_37 ~loc a0_ID a1_boption _c0_rule =
    let rec _c1_parameters ~loc x = state_45 ~loc x _c2_loption
    and _c2_loption ~loc x = state_46 ~loc x a0_ID a1_boption _c0_rule in
    match lookahead () with
    (* Shift *)
    | LPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_38 ~loc _c1_parameters
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a24 ~loc () in
      _c2_loption ~loc x
    | _ -> fail [ "COLON"; "LPAREN" ]

  (* ITEMS:
       parameters → LPAREN . loption RPAREN 		/ COLON
       symbol → . ID 		/ COMMA, RPAREN
       symbol → . TID 		/ COMMA, RPAREN
       separated_nonempty_list → . symbol COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . symbol 		/ RPAREN
       loption → . separated_nonempty_list 		/ RPAREN
       loption → . 		/ RPAREN
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 39
       separated_nonempty_list -> 42
       loption -> 43
     ACTION:
       ID TID -> shift
       RPAREN -> reduce 3 1 *)
  and state_38 ~loc _c0_parameters =
    let rec _c1_symbol ~loc x = state_39 ~loc x _c2_separated_nonempty_list
    and _c2_separated_nonempty_list ~loc x = state_42 ~loc x _c3_loption
    and _c3_loption ~loc x = state_43 ~loc x _c0_parameters in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a24 ~loc () in
      _c3_loption ~loc x
    | _ -> fail [ "ID"; "TID"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → symbol . COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → symbol . 		/ RPAREN
     GOTO:
       COMMA -> 40
     ACTION:
       COMMA -> shift
       RPAREN -> reduce 0 1 *)
  and state_39 ~loc a0_symbol _c0_separated_nonempty_list =
    match lookahead () with
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_40 ~loc a0_symbol _c0_separated_nonempty_list
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a22 ~loc (Actions.a21 ~loc a0_symbol ()) () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "COMMA"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → symbol COMMA . separated_nonempty_list 		/ RPAREN
       symbol → . ID 		/ COMMA, RPAREN
       symbol → . TID 		/ COMMA, RPAREN
       separated_nonempty_list → . symbol COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . symbol 		/ RPAREN
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 39
       separated_nonempty_list -> 41
     ACTION:
       ID TID -> shift *)
  and state_40 ~loc a1_symbol _c0_separated_nonempty_list =
    let rec _c1_symbol ~loc x = state_39 ~loc x _c2_separated_nonempty_list
    and _c2_separated_nonempty_list ~loc x = state_41 ~loc x a1_symbol _c0_separated_nonempty_list in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       separated_nonempty_list → symbol COMMA separated_nonempty_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_41 ~loc a0_separated_nonempty_list a2_symbol _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a23 ~loc a0_separated_nonempty_list () (Actions.a21 ~loc a2_symbol ()) () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       loption → separated_nonempty_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_42 ~loc a0_separated_nonempty_list _c0_loption =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a25 ~loc a0_separated_nonempty_list () in
      _c0_loption ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       parameters → LPAREN loption . RPAREN 		/ COLON
     GOTO:
       RPAREN -> 44
     ACTION:
       RPAREN -> shift *)
  and state_43 ~loc a0_loption _c0_parameters =
    match lookahead () with
    (* Shift *)
    | RPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_44 ~loc a0_loption _c0_parameters
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       parameters → LPAREN loption RPAREN . 		/ COLON
     GOTO:
       
     ACTION:
       COLON -> reduce 0 0 *)
  and state_44 ~loc a1_loption _c0_parameters =
    match lookahead () with
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a27 ~loc () (Actions.a26 ~loc a1_loption ()) () () in
      _c0_parameters ~loc x
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       loption → parameters . 		/ COLON
     GOTO:
       
     ACTION:
       COLON -> reduce 0 0 *)
  and state_45 ~loc a0_parameters _c0_loption =
    match lookahead () with
    (* Reduce *)
    | COLON ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a25 ~loc a0_parameters () in
      _c0_loption ~loc x
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → boption ID loption . COLON option separated_nonempty_list list 		/ ID, DINLINE, EOF
     GOTO:
       COLON -> 47
     ACTION:
       COLON -> shift *)
  and state_46 ~loc a0_loption a1_ID a2_boption _c0_rule =
    match lookahead () with
    (* Shift *)
    | COLON ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_47 ~loc a0_loption a1_ID a2_boption _c0_rule
    | _ -> fail [ "COLON" ]

  (* ITEMS:
       rule → boption ID loption COLON . option separated_nonempty_list list 		/ ID, DINLINE, EOF
       option → . BAR 		/ ID, TID, CODE, DWHEN, DPREC
       option → . 		/ ID, TID, CODE, DWHEN, DPREC
     GOTO:
       BAR -> 48
       option -> 49
     ACTION:
       BAR -> shift
       ID TID CODE DWHEN DPREC -> reduce 1 1 *)
  and state_47 ~loc a1_loption a2_ID a3_boption _c0_rule =
    let rec _c1_option ~loc x = state_49 ~loc x a1_loption a2_ID a3_boption _c0_rule in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_48 ~loc _c1_option
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a3 ~loc () in
      _c1_option ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "BAR" ]

  (* ITEMS:
       option → BAR . 		/ ID, TID, CODE, DWHEN, DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC -> reduce 0 0 *)
  and state_48 ~loc _c0_option =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a4 ~loc () () in
      _c0_option ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (* ITEMS:
       rule → boption ID loption COLON option . separated_nonempty_list list 		/ ID, DINLINE, EOF
       symbol → . ID 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR, LPAREN
       symbol → . TID 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR, LPAREN
       production → . list option actions 		/ ID, DINLINE, BAR, SEMI, EOF
       producer → . ID EQ actual list 		/ ID, TID, CODE, DWHEN, DPREC
       producer → . actual list 		/ ID, TID, CODE, DWHEN, DPREC
       actual → . actual shorthand 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       actual → . symbol loption 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       list → . producer list 		/ CODE, DWHEN, DPREC
       list → . 		/ CODE, DWHEN, DPREC
       separated_nonempty_list → . production BAR separated_nonempty_list 		/ ID, DINLINE, SEMI, EOF
       separated_nonempty_list → . production 		/ ID, DINLINE, SEMI, EOF
     GOTO:
       ID -> 50
       TID -> 11
       symbol -> 52
       production -> 76
       producer -> 54
       actual -> 55
       list -> 78
       separated_nonempty_list -> 95
     ACTION:
       CODE DWHEN DPREC -> reduce 5 1
       ID TID -> shift *)
  and state_49 ~loc a0_option a2_loption a3_ID a4_boption _c0_rule =
    let rec _c1_symbol ~loc x = state_52 ~loc x _c4_actual
    and _c2_production ~loc x = state_76 ~loc x _c6_separated_nonempty_list
    and _c3_producer ~loc x = state_54 ~loc x _c5_list
    and _c4_actual ~loc x = state_55 ~loc x _c3_producer _c4_actual
    and _c5_list ~loc x = state_78 ~loc x _c2_production
    and _c6_separated_nonempty_list ~loc x = state_95 ~loc x a0_option a2_loption a3_ID a4_boption _c0_rule in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c5_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc x _c1_symbol _c3_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (* ITEMS:
       symbol → ID . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN
       producer → ID . EQ actual list 		/ ID, TID, CODE, DWHEN, DPREC
     GOTO:
       EQ -> 51
     ACTION:
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR LPAREN RPAREN -> reduce 0 0
       EQ -> shift *)
  and state_50 ~loc a0_ID _c0_symbol _c1_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | LPAREN | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a11 ~loc a0_ID () in
      _c0_symbol ~loc x
    (* Shift *)
    | EQ ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_51 ~loc a0_ID _c1_producer
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "EQ"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       producer → ID EQ . actual list 		/ ID, TID, CODE, DWHEN, DPREC
       symbol → . ID 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR, LPAREN
       symbol → . TID 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR, LPAREN
       actual → . actual shorthand 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       actual → . symbol loption 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 52
       actual -> 74
     ACTION:
       ID TID -> shift *)
  and state_51 ~loc a1_ID _c0_producer =
    let rec _c1_symbol ~loc x = state_52 ~loc x _c2_actual
    and _c2_actual ~loc x = state_74 ~loc x a1_ID _c0_producer _c2_actual in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       actual → symbol . loption 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       args → . LPAREN separated_nonempty_list RPAREN 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       loption → . args 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       loption → . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       LPAREN -> 53
       args -> 72
       loption -> 73
     ACTION:
       LPAREN -> shift
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 2 1 *)
  and state_52 ~loc a0_symbol _c0_actual =
    let rec _c1_args ~loc x = state_72 ~loc x _c2_loption
    and _c2_loption ~loc x = state_73 ~loc x a0_symbol _c0_actual in
    match lookahead () with
    (* Shift *)
    | LPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_53 ~loc _c1_args
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a24 ~loc () in
      _c2_loption ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "LPAREN"; "RPAREN" ]

  (* ITEMS:
       args → LPAREN . separated_nonempty_list RPAREN 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       symbol → . ID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN
       symbol → . TID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN
       producer → . ID EQ actual list 		/ ID, TID, CODE
       producer → . actual list 		/ ID, TID, CODE
       actual → . actual shorthand 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       actual → . symbol loption 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       arg → . list CODE 		/ COMMA, RPAREN
       arg → . actual 		/ COMMA, RPAREN
       list → . producer list 		/ CODE
       list → . 		/ CODE
       separated_nonempty_list → . arg COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . arg 		/ RPAREN
     GOTO:
       ID -> 50
       TID -> 11
       symbol -> 52
       producer -> 54
       actual -> 64
       arg -> 65
       list -> 67
       separated_nonempty_list -> 70
     ACTION:
       CODE -> reduce 5 1
       ID TID -> shift *)
  and state_53 ~loc _c0_args =
    let rec _c1_symbol ~loc x = state_52 ~loc x _c3_actual
    and _c2_producer ~loc x = state_54 ~loc x _c5_list
    and _c3_actual ~loc x = state_64 ~loc x _c2_producer _c3_actual _c4_arg
    and _c4_arg ~loc x = state_65 ~loc x _c6_separated_nonempty_list
    and _c5_list ~loc x = state_67 ~loc x _c4_arg
    and _c6_separated_nonempty_list ~loc x = state_70 ~loc x _c0_args in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c5_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE" ]

  (* ITEMS:
       list → producer . list 		/ CODE, DWHEN, DPREC
       symbol → . ID 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR, LPAREN
       symbol → . TID 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR, LPAREN
       producer → . ID EQ actual list 		/ ID, TID, CODE, DWHEN, DPREC
       producer → . actual list 		/ ID, TID, CODE, DWHEN, DPREC
       actual → . actual shorthand 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       actual → . symbol loption 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       list → . producer list 		/ CODE, DWHEN, DPREC
       list → . 		/ CODE, DWHEN, DPREC
     GOTO:
       ID -> 50
       TID -> 11
       symbol -> 52
       producer -> 54
       actual -> 55
       list -> 63
     ACTION:
       CODE DWHEN DPREC -> reduce 4 1
       ID TID -> shift *)
  and state_54 ~loc a0_producer _c0_list =
    let rec _c1_symbol ~loc x = state_52 ~loc x _c3_actual
    and _c2_producer ~loc x = state_54 ~loc x _c4_list
    and _c3_actual ~loc x = state_55 ~loc x _c2_producer _c3_actual
    and _c4_list ~loc x = state_63 ~loc x a0_producer _c0_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (* ITEMS:
       producer → actual . list 		/ ID, TID, CODE, DWHEN, DPREC
       actual → actual . shorthand 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       shorthand → . PLUS 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       shorthand → . STAR 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       shorthand → . QMARK 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       list → . SEMI list 		/ ID, TID, CODE, DWHEN, DPREC
       list → . 		/ ID, TID, CODE, DWHEN, DPREC
     GOTO:
       PLUS -> 56
       QMARK -> 57
       SEMI -> 58
       STAR -> 60
       shorthand -> 61
       list -> 62
     ACTION:
       PLUS QMARK SEMI STAR -> shift
       ID TID CODE DWHEN DPREC -> reduce 3 1 *)
  and state_55 ~loc a0_actual _c0_producer _c1_actual =
    let rec _c2_shorthand ~loc x = state_61 ~loc x a0_actual _c1_actual
    and _c3_list ~loc x = state_62 ~loc x a0_actual _c0_producer in
    match lookahead () with
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_56 ~loc _c2_shorthand
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_57 ~loc _c2_shorthand
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_58 ~loc _c3_list
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_60 ~loc _c2_shorthand
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c3_list ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "PLUS"; "QMARK"; "SEMI"; "STAR" ]

  (* ITEMS:
       shorthand → PLUS . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0 *)
  and state_56 ~loc _c0_shorthand =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a31 ~loc () () in
      _c0_shorthand ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (* ITEMS:
       shorthand → QMARK . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0 *)
  and state_57 ~loc _c0_shorthand =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a33 ~loc () () in
      _c0_shorthand ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (* ITEMS:
       list → SEMI . list 		/ ID, TID, CODE, DWHEN, DINLINE, DPREC, EOF
       list → . SEMI list 		/ ID, TID, CODE, DWHEN, DINLINE, DPREC, EOF
       list → . 		/ ID, TID, CODE, DWHEN, DINLINE, DPREC, EOF
     GOTO:
       SEMI -> 58
       list -> 59
     ACTION:
       SEMI -> shift
       ID TID CODE DWHEN DINLINE DPREC EOF -> reduce 1 1 *)
  and state_58 ~loc _c0_list =
    let rec _c1_list ~loc x = state_59 ~loc x _c0_list in
    match lookahead () with
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_58 ~loc _c1_list
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DINLINE | DPREC | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DINLINE"; "DPREC"; "SEMI"; "EOF" ]

  (* ITEMS:
       list → SEMI list . 		/ ID, TID, CODE, DWHEN, DINLINE, DPREC, EOF
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DINLINE DPREC EOF -> reduce 0 0 *)
  and state_59 ~loc a0_list _c0_list =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DINLINE | DPREC | EOF ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list () () in
      _c0_list ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DINLINE"; "DPREC"; "EOF" ]

  (* ITEMS:
       shorthand → STAR . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0 *)
  and state_60 ~loc _c0_shorthand =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a32 ~loc () () in
      _c0_shorthand ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (* ITEMS:
       actual → actual shorthand . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0 *)
  and state_61 ~loc a0_shorthand a1_actual _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a34 ~loc a0_shorthand a1_actual () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (* ITEMS:
       producer → actual list . 		/ ID, TID, CODE, DWHEN, DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC -> reduce 0 0 *)
  and state_62 ~loc a0_list a1_actual _c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a40 ~loc a0_list a1_actual (Actions.a29 ~loc ()) () in
      _c0_producer ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (* ITEMS:
       list → producer list . 		/ CODE, DWHEN, DPREC
     GOTO:
       
     ACTION:
       CODE DWHEN DPREC -> reduce 0 0 *)
  and state_63 ~loc a0_list a1_producer _c0_list =
    match lookahead () with
    (* Reduce *)
    | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_producer () in
      _c0_list ~loc x
    | _ -> fail [ "CODE"; "DWHEN"; "DPREC" ]

  (* ITEMS:
       producer → actual . list 		/ ID, TID, CODE
       actual → actual . shorthand 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       arg → actual . 		/ COMMA, RPAREN
       shorthand → . PLUS 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       shorthand → . STAR 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       shorthand → . QMARK 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       list → . SEMI list 		/ ID, TID, CODE
       list → . 		/ ID, TID, CODE
     GOTO:
       PLUS -> 56
       QMARK -> 57
       SEMI -> 58
       STAR -> 60
       shorthand -> 61
       list -> 62
     ACTION:
       ID TID CODE -> reduce 4 1
       PLUS QMARK SEMI STAR -> shift
       COMMA RPAREN -> reduce 2 0 *)
  and state_64 ~loc a0_actual _c0_producer _c1_actual _c2_arg =
    let rec _c3_shorthand ~loc x = state_61 ~loc x a0_actual _c1_actual
    and _c4_list ~loc x = state_62 ~loc x a0_actual _c0_producer in
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_56 ~loc _c3_shorthand
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_57 ~loc _c3_shorthand
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_58 ~loc _c4_list
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_60 ~loc _c3_shorthand
    (* Reduce *)
    | COMMA | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a35 ~loc a0_actual () in
      _c2_arg ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → arg . COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → arg . 		/ RPAREN
     GOTO:
       COMMA -> 66
     ACTION:
       COMMA -> shift
       RPAREN -> reduce 0 1 *)
  and state_65 ~loc a0_arg _c0_separated_nonempty_list =
    match lookahead () with
    (* Shift *)
    | COMMA ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_66 ~loc a0_arg _c0_separated_nonempty_list
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a22 ~loc a0_arg () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "COMMA"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → arg COMMA . separated_nonempty_list 		/ RPAREN
       symbol → . ID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN
       symbol → . TID 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN
       producer → . ID EQ actual list 		/ ID, TID, CODE
       producer → . actual list 		/ ID, TID, CODE
       actual → . actual shorthand 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       actual → . symbol loption 		/ ID, TID, CODE, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
       arg → . list CODE 		/ COMMA, RPAREN
       arg → . actual 		/ COMMA, RPAREN
       list → . producer list 		/ CODE
       list → . 		/ CODE
       separated_nonempty_list → . arg COMMA separated_nonempty_list 		/ RPAREN
       separated_nonempty_list → . arg 		/ RPAREN
     GOTO:
       ID -> 50
       TID -> 11
       symbol -> 52
       producer -> 54
       actual -> 64
       arg -> 65
       list -> 67
       separated_nonempty_list -> 69
     ACTION:
       CODE -> reduce 5 1
       ID TID -> shift *)
  and state_66 ~loc a1_arg _c0_separated_nonempty_list =
    let rec _c1_symbol ~loc x = state_52 ~loc x _c3_actual
    and _c2_producer ~loc x = state_54 ~loc x _c5_list
    and _c3_actual ~loc x = state_64 ~loc x _c2_producer _c3_actual _c4_arg
    and _c4_arg ~loc x = state_65 ~loc x _c6_separated_nonempty_list
    and _c5_list ~loc x = state_67 ~loc x _c4_arg
    and _c6_separated_nonempty_list ~loc x = state_69 ~loc x a1_arg _c0_separated_nonempty_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c5_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE" ]

  (* ITEMS:
       arg → list . CODE 		/ COMMA, RPAREN
     GOTO:
       CODE -> 68
     ACTION:
       CODE -> shift *)
  and state_67 ~loc a0_list _c0_arg =
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_68 ~loc x a0_list _c0_arg
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       arg → list CODE . 		/ COMMA, RPAREN
     GOTO:
       
     ACTION:
       COMMA RPAREN -> reduce 0 0 *)
  and state_68 ~loc a0_CODE a1_list _c0_arg =
    match lookahead () with
    (* Reduce *)
    | COMMA | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a37 ~loc (Actions.a36 ~loc (Actions.a0 ~loc a0_CODE ()) ()) a1_list () in
      _c0_arg ~loc x
    | _ -> fail [ "COMMA"; "RPAREN" ]

  (* ITEMS:
       separated_nonempty_list → arg COMMA separated_nonempty_list . 		/ RPAREN
     GOTO:
       
     ACTION:
       RPAREN -> reduce 0 0 *)
  and state_69 ~loc a0_separated_nonempty_list a2_arg _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a23 ~loc a0_separated_nonempty_list () a2_arg () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       args → LPAREN separated_nonempty_list . RPAREN 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       RPAREN -> 71
     ACTION:
       RPAREN -> shift *)
  and state_70 ~loc a0_separated_nonempty_list _c0_args =
    match lookahead () with
    (* Shift *)
    | RPAREN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_71 ~loc a0_separated_nonempty_list _c0_args
    | _ -> fail [ "RPAREN" ]

  (* ITEMS:
       args → LPAREN separated_nonempty_list RPAREN . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0 *)
  and state_71 ~loc a1_separated_nonempty_list _c0_args =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a38 ~loc () a1_separated_nonempty_list () () in
      _c0_args ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (* ITEMS:
       loption → args . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0 *)
  and state_72 ~loc a0_args _c0_loption =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a25 ~loc a0_args () in
      _c0_loption ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (* ITEMS:
       actual → symbol loption . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0 *)
  and state_73 ~loc a0_loption a1_symbol _c0_actual =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a39 ~loc a0_loption a1_symbol () in
      _c0_actual ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (* ITEMS:
       producer → ID EQ actual . list 		/ ID, TID, CODE, DWHEN, DPREC
       actual → actual . shorthand 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       shorthand → . PLUS 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       shorthand → . STAR 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       shorthand → . QMARK 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       list → . SEMI list 		/ ID, TID, CODE, DWHEN, DPREC
       list → . 		/ ID, TID, CODE, DWHEN, DPREC
     GOTO:
       PLUS -> 56
       QMARK -> 57
       SEMI -> 58
       STAR -> 60
       shorthand -> 61
       list -> 75
     ACTION:
       PLUS QMARK SEMI STAR -> shift
       ID TID CODE DWHEN DPREC -> reduce 3 1 *)
  and state_74 ~loc a0_actual a2_ID _c0_producer _c1_actual =
    let rec _c2_shorthand ~loc x = state_61 ~loc x a0_actual _c1_actual
    and _c3_list ~loc x = state_75 ~loc x a0_actual a2_ID _c0_producer in
    match lookahead () with
    (* Shift *)
    | PLUS ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_56 ~loc _c2_shorthand
    (* Shift *)
    | QMARK ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_57 ~loc _c2_shorthand
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_58 ~loc _c3_list
    (* Shift *)
    | STAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_60 ~loc _c2_shorthand
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c3_list ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "PLUS"; "QMARK"; "SEMI"; "STAR" ]

  (* ITEMS:
       producer → ID EQ actual list . 		/ ID, TID, CODE, DWHEN, DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DWHEN DPREC -> reduce 0 0 *)
  and state_75 ~loc a0_list a1_actual a3_ID _c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 4
      and x = Actions.a40 ~loc a0_list a1_actual (Actions.a30 ~loc (Actions.a28 ~loc () (Actions.a9 ~loc (Actions.a0 ~loc a3_ID ()) ()) ()) ()) () in
      _c0_producer ~loc x
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (* ITEMS:
       separated_nonempty_list → production . BAR separated_nonempty_list 		/ ID, DINLINE, SEMI, EOF
       separated_nonempty_list → production . 		/ ID, DINLINE, SEMI, EOF
     GOTO:
       BAR -> 77
     ACTION:
       BAR -> shift
       ID DINLINE SEMI EOF -> reduce 0 1 *)
  and state_76 ~loc a0_production _c0_separated_nonempty_list =
    match lookahead () with
    (* Shift *)
    | BAR ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_77 ~loc a0_production _c0_separated_nonempty_list
    (* Reduce *)
    | ID _ | DINLINE | SEMI | EOF ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a22 ~loc a0_production () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (* ITEMS:
       separated_nonempty_list → production BAR . separated_nonempty_list 		/ ID, DINLINE, SEMI, EOF
       symbol → . ID 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR, LPAREN
       symbol → . TID 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR, LPAREN
       production → . list option actions 		/ ID, DINLINE, BAR, SEMI, EOF
       producer → . ID EQ actual list 		/ ID, TID, CODE, DWHEN, DPREC
       producer → . actual list 		/ ID, TID, CODE, DWHEN, DPREC
       actual → . actual shorthand 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       actual → . symbol loption 		/ ID, TID, CODE, DWHEN, DPREC, PLUS, QMARK, SEMI, STAR
       list → . producer list 		/ CODE, DWHEN, DPREC
       list → . 		/ CODE, DWHEN, DPREC
       separated_nonempty_list → . production BAR separated_nonempty_list 		/ ID, DINLINE, SEMI, EOF
       separated_nonempty_list → . production 		/ ID, DINLINE, SEMI, EOF
     GOTO:
       ID -> 50
       TID -> 11
       symbol -> 52
       production -> 76
       producer -> 54
       actual -> 55
       list -> 78
       separated_nonempty_list -> 94
     ACTION:
       CODE DWHEN DPREC -> reduce 5 1
       ID TID -> shift *)
  and state_77 ~loc a1_production _c0_separated_nonempty_list =
    let rec _c1_symbol ~loc x = state_52 ~loc x _c4_actual
    and _c2_production ~loc x = state_76 ~loc x _c6_separated_nonempty_list
    and _c3_producer ~loc x = state_54 ~loc x _c5_list
    and _c4_actual ~loc x = state_55 ~loc x _c3_producer _c4_actual
    and _c5_list ~loc x = state_78 ~loc x _c2_production
    and _c6_separated_nonempty_list ~loc x = state_94 ~loc x a1_production _c0_separated_nonempty_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DWHEN | DPREC ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c5_list ~loc x
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_50 ~loc x _c1_symbol _c3_producer
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (* ITEMS:
       production → list . option actions 		/ ID, DINLINE, BAR, SEMI, EOF
       option → . DPREC ident 		/ CODE, DWHEN
       option → . 		/ CODE, DWHEN
     GOTO:
       DPREC -> 79
       option -> 81
     ACTION:
       DPREC -> shift
       CODE DWHEN -> reduce 1 1 *)
  and state_78 ~loc a0_list _c0_production =
    let rec _c1_option ~loc x = state_81 ~loc x a0_list _c0_production in
    match lookahead () with
    (* Shift *)
    | DPREC ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_79 ~loc _c1_option
    (* Reduce *)
    | CODE _ | DWHEN ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a3 ~loc () in
      _c1_option ~loc x
    | _ -> fail [ "CODE"; "DWHEN"; "DPREC" ]

  (* ITEMS:
       option → DPREC . ident 		/ CODE, DWHEN
       ident → . ID 		/ CODE, DWHEN
       ident → . TID 		/ CODE, DWHEN
     GOTO:
       ID -> 21
       TID -> 22
       ident -> 80
     ACTION:
       ID TID -> shift *)
  and state_79 ~loc _c0_option =
    let rec _c1_ident ~loc x = state_80 ~loc x _c0_option in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_21 ~loc x _c1_ident
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_22 ~loc x _c1_ident
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       option → DPREC ident . 		/ CODE, DWHEN
     GOTO:
       
     ACTION:
       CODE DWHEN -> reduce 0 0 *)
  and state_80 ~loc a0_ident _c0_option =
    match lookahead () with
    (* Reduce *)
    | CODE _ | DWHEN ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a4 ~loc (Actions.a41 ~loc a0_ident () ()) () in
      _c0_option ~loc x
    | _ -> fail [ "CODE"; "DWHEN" ]

  (* ITEMS:
       production → list option . actions 		/ ID, DINLINE, BAR, SEMI, EOF
       actions → . list CODE 		/ ID, DINLINE, BAR, SEMI, EOF
       actions → . nonempty_list 		/ ID, DINLINE, BAR, SEMI, EOF
       conditional_action → . DWHEN symbol EQ symbol CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
       nonempty_list → . conditional_action nonempty_list 		/ ID, DINLINE, BAR, SEMI, EOF
       nonempty_list → . conditional_action 		/ ID, DINLINE, BAR, SEMI, EOF
       list → . conditional_action list 		/ CODE
       list → . 		/ CODE
     GOTO:
       DWHEN -> 82
       actions -> 87
       conditional_action -> 88
       nonempty_list -> 91
       list -> 92
     ACTION:
       CODE -> reduce 4 1
       DWHEN -> shift *)
  and state_81 ~loc a0_option a1_list _c0_production =
    let rec _c1_actions ~loc x = state_87 ~loc x a0_option a1_list _c0_production
    and _c2_conditional_action ~loc x = state_88 ~loc x _c3_nonempty_list _c4_list
    and _c3_nonempty_list ~loc x = state_91 ~loc x _c1_actions
    and _c4_list ~loc x = state_92 ~loc x _c1_actions in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | DWHEN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_82 ~loc _c2_conditional_action
    | _ -> fail [ "CODE"; "DWHEN" ]

  (* ITEMS:
       conditional_action → DWHEN . symbol EQ symbol CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
       symbol → . ID 		/ EQ
       symbol → . TID 		/ EQ
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 83
     ACTION:
       ID TID -> shift *)
  and state_82 ~loc _c0_conditional_action =
    let rec _c1_symbol ~loc x = state_83 ~loc x _c0_conditional_action in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       conditional_action → DWHEN symbol . EQ symbol CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
     GOTO:
       EQ -> 84
     ACTION:
       EQ -> shift *)
  and state_83 ~loc a0_symbol _c0_conditional_action =
    match lookahead () with
    (* Shift *)
    | EQ ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_84 ~loc a0_symbol _c0_conditional_action
    | _ -> fail [ "EQ" ]

  (* ITEMS:
       conditional_action → DWHEN symbol EQ . symbol CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
       symbol → . ID 		/ CODE
       symbol → . TID 		/ CODE
     GOTO:
       ID -> 10
       TID -> 11
       symbol -> 85
     ACTION:
       ID TID -> shift *)
  and state_84 ~loc a1_symbol _c0_conditional_action =
    let rec _c1_symbol ~loc x = state_85 ~loc x a1_symbol _c0_conditional_action in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_10 ~loc x _c1_symbol
    (* Shift *)
    | TID x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_11 ~loc x _c1_symbol
    | _ -> fail [ "ID"; "TID" ]

  (* ITEMS:
       conditional_action → DWHEN symbol EQ symbol . CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
     GOTO:
       CODE -> 86
     ACTION:
       CODE -> shift *)
  and state_85 ~loc a0_symbol a2_symbol _c0_conditional_action =
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_86 ~loc x a0_symbol a2_symbol _c0_conditional_action
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       conditional_action → DWHEN symbol EQ symbol CODE . 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
     GOTO:
       
     ACTION:
       ID CODE DWHEN DINLINE BAR SEMI EOF -> reduce 0 0 *)
  and state_86 ~loc a0_CODE a1_symbol a3_symbol _c0_conditional_action =
    match lookahead () with
    (* Reduce *)
    | ID _ | CODE _ | DWHEN | DINLINE | BAR | SEMI | EOF ->
      let loc = loc_reduce ~loc 5
      and x = Actions.a42 ~loc (Actions.a36 ~loc (Actions.a0 ~loc a0_CODE ()) ()) a1_symbol () a3_symbol () () in
      _c0_conditional_action ~loc x
    | _ -> fail [ "ID"; "CODE"; "DWHEN"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (* ITEMS:
       production → list option actions . 		/ ID, DINLINE, BAR, SEMI, EOF
     GOTO:
       
     ACTION:
       ID DINLINE BAR SEMI EOF -> reduce 0 0 *)
  and state_87 ~loc a0_actions a1_option a2_list _c0_production =
    match lookahead () with
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a47 ~loc a0_actions a1_option a2_list () in
      _c0_production ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (* ITEMS:
       nonempty_list → conditional_action . nonempty_list 		/ ID, DINLINE, BAR, SEMI, EOF
       nonempty_list → conditional_action . 		/ ID, DINLINE, BAR, SEMI, EOF
       list → conditional_action . list 		/ CODE
       conditional_action → . DWHEN symbol EQ symbol CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
       nonempty_list → . conditional_action nonempty_list 		/ ID, DINLINE, BAR, SEMI, EOF
       nonempty_list → . conditional_action 		/ ID, DINLINE, BAR, SEMI, EOF
       list → . conditional_action list 		/ CODE
       list → . 		/ CODE
     GOTO:
       DWHEN -> 82
       conditional_action -> 88
       nonempty_list -> 89
       list -> 90
     ACTION:
       CODE -> reduce 4 1
       DWHEN -> shift
       ID DINLINE BAR SEMI EOF -> reduce 0 1 *)
  and state_88 ~loc a0_conditional_action _c0_nonempty_list _c1_list =
    let rec _c2_conditional_action ~loc x = state_88 ~loc x _c3_nonempty_list _c4_list
    and _c3_nonempty_list ~loc x = state_89 ~loc x a0_conditional_action _c0_nonempty_list
    and _c4_list ~loc x = state_90 ~loc x a0_conditional_action _c1_list in
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c4_list ~loc x
    (* Shift *)
    | DWHEN ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_82 ~loc _c2_conditional_action
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a43 ~loc a0_conditional_action () in
      _c0_nonempty_list ~loc x
    | _ -> fail [ "ID"; "CODE"; "DWHEN"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (* ITEMS:
       nonempty_list → conditional_action nonempty_list . 		/ ID, DINLINE, BAR, SEMI, EOF
     GOTO:
       
     ACTION:
       ID DINLINE BAR SEMI EOF -> reduce 0 0 *)
  and state_89 ~loc a0_nonempty_list a1_conditional_action _c0_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a44 ~loc a0_nonempty_list a1_conditional_action () in
      _c0_nonempty_list ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (* ITEMS:
       list → conditional_action list . 		/ CODE
     GOTO:
       
     ACTION:
       CODE -> reduce 0 0 *)
  and state_90 ~loc a0_list a1_conditional_action _c0_list =
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_conditional_action () in
      _c0_list ~loc x
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       actions → nonempty_list . 		/ ID, DINLINE, BAR, SEMI, EOF
     GOTO:
       
     ACTION:
       ID DINLINE BAR SEMI EOF -> reduce 0 0 *)
  and state_91 ~loc a0_nonempty_list _c0_actions =
    match lookahead () with
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let loc = loc_reduce ~loc 1
      and x = Actions.a45 ~loc a0_nonempty_list () in
      _c0_actions ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (* ITEMS:
       actions → list . CODE 		/ ID, DINLINE, BAR, SEMI, EOF
     GOTO:
       CODE -> 93
     ACTION:
       CODE -> shift *)
  and state_92 ~loc a0_list _c0_actions =
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_93 ~loc x a0_list _c0_actions
    | _ -> fail [ "CODE" ]

  (* ITEMS:
       actions → list CODE . 		/ ID, DINLINE, BAR, SEMI, EOF
     GOTO:
       
     ACTION:
       ID DINLINE BAR SEMI EOF -> reduce 0 0 *)
  and state_93 ~loc a0_CODE a1_list _c0_actions =
    match lookahead () with
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a46 ~loc (Actions.a36 ~loc (Actions.a0 ~loc a0_CODE ()) ()) a1_list () in
      _c0_actions ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (* ITEMS:
       separated_nonempty_list → production BAR separated_nonempty_list . 		/ ID, DINLINE, SEMI, EOF
     GOTO:
       
     ACTION:
       ID DINLINE SEMI EOF -> reduce 0 0 *)
  and state_94 ~loc a0_separated_nonempty_list a2_production _c0_separated_nonempty_list =
    match lookahead () with
    (* Reduce *)
    | ID _ | DINLINE | SEMI | EOF ->
      let loc = loc_reduce ~loc 3
      and x = Actions.a23 ~loc a0_separated_nonempty_list () a2_production () in
      _c0_separated_nonempty_list ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "SEMI"; "EOF" ]

  (* ITEMS:
       rule → boption ID loption COLON option separated_nonempty_list . list 		/ ID, DINLINE, EOF
       list → . SEMI list 		/ ID, DINLINE, EOF
       list → . 		/ ID, DINLINE, EOF
     GOTO:
       SEMI -> 58
       list -> 96
     ACTION:
       SEMI -> shift
       ID DINLINE EOF -> reduce 1 1 *)
  and state_95 ~loc a0_separated_nonempty_list a1_option a3_loption a4_ID a5_boption _c0_rule =
    let rec _c1_list ~loc x = state_96 ~loc x a0_separated_nonempty_list a1_option a3_loption a4_ID a5_boption _c0_rule in
    match lookahead () with
    (* Shift *)
    | SEMI ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_58 ~loc _c1_list
    (* Reduce *)
    | ID _ | DINLINE | EOF ->
      let loc = loc_reduce ~loc 0
      and x = Actions.a6 ~loc () in
      _c1_list ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "SEMI"; "EOF" ]

  (* ITEMS:
       rule → boption ID loption COLON option separated_nonempty_list list . 		/ ID, DINLINE, EOF
     GOTO:
       
     ACTION:
       ID DINLINE EOF -> reduce 0 0 *)
  and state_96 ~loc a0_list a1_separated_nonempty_list a2_option a4_loption a5_ID a6_boption _c0_rule =
    match lookahead () with
    (* Reduce *)
    | ID _ | DINLINE | EOF ->
      let loc = loc_reduce ~loc 7
      and x = Actions.a48 ~loc a0_list a1_separated_nonempty_list a2_option () a4_loption (Actions.a9 ~loc (Actions.a0 ~loc a5_ID ()) ()) a6_boption () in
      _c0_rule ~loc x
    | _ -> fail [ "ID"; "DINLINE"; "EOF" ]

  (* ITEMS:
       list → rule list . 		/ EOF
     GOTO:
       
     ACTION:
       EOF -> reduce 0 0 *)
  and state_97 ~loc a0_list a1_rule _c0_list =
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let loc = loc_reduce ~loc 2
      and x = Actions.a7 ~loc a0_list a1_rule () in
      _c0_list ~loc x
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       grammar' → list DSEP list . EOF
     GOTO:
       EOF -> 99
     ACTION:
       EOF -> shift *)
  and state_98 ~loc a0_list a2_list _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | EOF ->
      let _, _l = shift () in
      let loc = loc_shift ~loc _l in
      state_99 ~loc a0_list a2_list _c0_grammar_starting
    | _ -> fail [ "EOF" ]

  (* ITEMS:
       grammar' → list DSEP list EOF .
     GOTO:
       
     ACTION:
        *)
  and state_99 ~loc a1_list a3_list _c0_grammar_starting =
    (* Reduce *)
    let x = Actions.a49 ~loc () a1_list () a3_list () in
    _c0_grammar_starting x
  ;;
end

let grammar lexfun lexbuf =
  States.setup lexfun lexbuf;
  States.state_0 ~loc:[] (fun x -> x)
;;

let error_token () = !States.error_token
let expected_tokens () = !States.expected_tokens
