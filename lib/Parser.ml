[@@@warning "-unused-rec-flag"]
[@@@warning "-redundant-case"]
[@@@warning "-redundant-subpat"]

module ParsingCompat = struct
  let set_loc _ _ = ()
end

open Raw

let plus, star, qmark =
  let span = Lexing.dummy_pos, Lexing.dummy_pos in
  let sym data = { span; data = NTerm data } in
  sym "nonempty_list", sym "list", sym "option"
;;

type token =
  | BAR
  | CODE of (Raw.code)
  | COLON
  | COMMA
  | DCODE of (string)
  | DINLINE
  | DLEFT
  | DNONASSOC
  | DPREC
  | DRIGHT
  | DSEP
  | DSTART
  | DTOKEN
  | DTYPE
  | DWHEN
  | EOF
  | EQ
  | ID of (string)
  | LPAREN
  | PLUS
  | QMARK
  | RPAREN
  | SEMI
  | STAR
  | TID of (string)
  | TYPE of (string)
;;

module Actions = struct
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

  let a0 ~_loc data () = ( { span = (_kw_loc ~_loc 1); data } )
  and a1 ~_loc data () = (DeclCode data)
  and a2 ~_loc _arg1 () = ( _arg1 )
  and a3 ~_loc () = (None)
  and a4 ~_loc x () = (Some x)
  and a5 ~_loc _arg1 () = ( _arg1 )
  and a6 ~_loc () = ([])
  and a7 ~_loc xs x () = (x :: xs)
  and a8 ~_loc xs tp _arg1 () = (DeclToken (tp, xs))
  and a9 ~_loc _arg1 () = ( _arg1 )
  and a10 ~_loc xs tp _arg1 () = (DeclStart (tp, xs))
  and a11 ~_loc x () = ( { span = (_kw_loc ~_loc 1); data = NTerm x } )
  and a12 ~_loc x () = ( { span = (_kw_loc ~_loc 1); data = Term x } )
  and a13 ~_loc xs tp _arg1 () = (DeclType  (tp, xs))
  and a14 ~_loc x () = (x)
  and a15 ~_loc x () = (x)
  and a16 ~_loc xs _arg1 () = (DeclLeft xs)
  and a17 ~_loc xs _arg1 () = (DeclRight xs)
  and a18 ~_loc xs _arg1 () = (DeclNonassoc xs)
  and a19 ~_loc () = (false)
  and a20 ~_loc _arg1 () = (true)
  and a21 ~_loc x () = (x)
  and a22 ~_loc x () = ([ x ])
  and a23 ~_loc xs _arg2 x () = (x :: xs)
  and a24 ~_loc () = ([])
  and a25 ~_loc x () = (x)
  and a26 ~_loc xs () = (xs)
  and a27 ~_loc _arg3 params _arg1 () = (params)
  and a28 ~_loc _arg2 x () = (x)
  and a29 ~_loc () = (None)
  and a30 ~_loc x () = (Some x)
  and a31 ~_loc _arg1 () = (plus)
  and a32 ~_loc _arg1 () = (star)
  and a33 ~_loc _arg1 () = (qmark)
  and a34 ~_loc a_symbol a_actual () = ({ a_symbol; a_args = [ Arg a_actual ] })
  and a35 ~_loc x () = (Arg x)
  and a36 ~_loc _arg1 () = ( _arg1 )
  and a37 ~_loc a_action a_prod () = (ArgInline { a_prod; a_action })
  and a38 ~_loc _arg3 args _arg1 () = (args)
  and a39 ~_loc a_args a_symbol () = ({ a_symbol; a_args })
  and a40 ~_loc _arg3 p_actual p_id () = ({ p_id; p_actual })
  and a41 ~_loc x _arg1 () = (x)
  and a42 ~_loc a_code rhs _arg3 lhs _arg1 () = ({ a_cond = Some ((lhs, rhs)); a_code })
  and a43 ~_loc x () = ([ x ])
  and a44 ~_loc xs x () = (x :: xs)
  and a45 ~_loc xs () = (xs)
  and a46 ~_loc a_code xs () = (xs @ [{ a_cond = None; a_code }])
  and a47 ~_loc p_actions p_prec p_prod () = ({ p_prod; p_prec; p_actions })
  and a48 ~_loc _arg7 r_prods _arg5 _arg4 r_params r_id r_inline () = ({ r_id; r_inline; r_params; r_prods })
  and a49 ~_loc _arg4 r_rules _arg2 r_decls () = ({ r_decls; r_rules })
end

module States = struct
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
      let rec drop n xs = if n <= 0 then xs else drop (n - 1) (List.tl xs) in
      let l = fst (List.nth _loc (n - 1)), snd (List.hd _loc) in
      ParsingCompat.set_loc _loc n;
      l :: drop n _loc
  ;;

  (*
    ITEMS:
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
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift
  *)
  let rec state_0 ~_loc t _c0_grammar_starting = 
    let rec _c1_decl ~_loc t x = state_30 ~_loc t x _c2_list
    and _c2_list ~_loc t x = state_32 ~_loc t x _c0_grammar_starting in
    match fst t with
    (* Reduce *)
    | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c2_list ~_loc t x
    (* Shift *)
    | DCODE x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_1 ~_loc t x _c1_decl
    (* Shift *)
    | DTOKEN ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_2 ~_loc t _c1_decl
    (* Shift *)
    | DTYPE ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_8 ~_loc t _c1_decl
    (* Shift *)
    | DSTART ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_15 ~_loc t _c1_decl
    (* Shift *)
    | DLEFT ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_20 ~_loc t _c1_decl
    (* Shift *)
    | DRIGHT ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_26 ~_loc t _c1_decl
    (* Shift *)
    | DNONASSOC ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_28 ~_loc t _c1_decl
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DCODE . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_1 ~_loc t a0_DCODE _c0_decl = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a1 ~_loc (Actions.a0 ~_loc a0_DCODE ()) () in
      _c0_decl ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DTOKEN . option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      option → . TYPE 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      option → . 		/ TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    GOTO:
      TYPE -> 3
      option -> 4

    ACTION:
      TYPE -> shift
      TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
  *)
  and state_2 ~_loc t _c0_decl = 
    let rec _c1_option ~_loc t x = state_4 ~_loc t x _c0_decl in
    match fst t with
    (* Shift *)
    | TYPE x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_3 ~_loc t x _c1_option
    (* Reduce *)
    | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a3 ~_loc () in
      _c1_option ~_loc t x
    | _ ->
      fail t [ "TID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      option → TYPE . 		/ ID, TID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      ID TID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_3 ~_loc t a0_TYPE _c0_option = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a4 ~_loc (Actions.a2 ~_loc (Actions.a0 ~_loc a0_TYPE ()) ()) () in
      _c0_option ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DTOKEN option . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      list → . TID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    GOTO:
      TID -> 5
      list -> 7

    ACTION:
      TID -> shift
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
  *)
  and state_4 ~_loc t a0_option _c0_decl = 
    let rec _c1_list ~_loc t x = state_7 ~_loc t x a0_option _c0_decl in
    match fst t with
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_5 ~_loc t x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c1_list ~_loc t x
    | _ ->
      fail t [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      list → TID . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      list → . TID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    GOTO:
      TID -> 5
      list -> 6

    ACTION:
      TID -> shift
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
  *)
  and state_5 ~_loc t a0_TID _c0_list = 
    let rec _c1_list ~_loc t x = state_6 ~_loc t x a0_TID _c0_list in
    match fst t with
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_5 ~_loc t x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c1_list ~_loc t x
    | _ ->
      fail t [ "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      list → TID list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_6 ~_loc t a0_list a1_TID _c0_list = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list (Actions.a5 ~_loc (Actions.a0 ~_loc a1_TID ()) ()) () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DTOKEN option list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_7 ~_loc t a0_list a1_option _c0_decl = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a8 ~_loc a0_list a1_option () () in
      _c0_decl ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DTYPE . TYPE list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    GOTO:
      TYPE -> 9

    ACTION:
      TYPE -> shift
  *)
  and state_8 ~_loc t _c0_decl = 
    match fst t with
    (* Shift *)
    | TYPE x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_9 ~_loc t x _c0_decl
    | _ ->
      fail t [ "TYPE" ]

  (*
    ITEMS:
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
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
  *)
  and state_9 ~_loc t a0_TYPE _c0_decl = 
    let rec _c1_symbol ~_loc t x = state_12 ~_loc t x _c2_list
    and _c2_list ~_loc t x = state_14 ~_loc t x a0_TYPE _c0_decl in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_10 ~_loc t x _c1_symbol
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c2_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      symbol → ID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DWHEN, DPREC, COMMA, EQ, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN

    ACTION:
      ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN DPREC COMMA EQ PLUS QMARK SEMI STAR LPAREN RPAREN -> reduce 0 0
  *)
  and state_10 ~_loc t a0_ID _c0_symbol = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DWHEN | DPREC | COMMA | EQ | PLUS | QMARK | SEMI | STAR | LPAREN | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a11 ~_loc a0_ID () in
      _c0_symbol ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DWHEN"; "DPREC"; "COMMA"; "EQ"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "LPAREN"; "RPAREN" ]

  (*
    ITEMS:
      symbol → TID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DWHEN, DPREC, COMMA, EQ, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN

    ACTION:
      ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN DPREC COMMA EQ PLUS QMARK SEMI STAR LPAREN RPAREN -> reduce 0 0
  *)
  and state_11 ~_loc t a0_TID _c0_symbol = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DWHEN | DPREC | COMMA | EQ | PLUS | QMARK | SEMI | STAR | LPAREN | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a12 ~_loc a0_TID () in
      _c0_symbol ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DWHEN"; "DPREC"; "COMMA"; "EQ"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "LPAREN"; "RPAREN" ]

  (*
    ITEMS:
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
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
  *)
  and state_12 ~_loc t a0_symbol _c0_list = 
    let rec _c1_symbol ~_loc t x = state_12 ~_loc t x _c2_list
    and _c2_list ~_loc t x = state_13 ~_loc t x a0_symbol _c0_list in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_10 ~_loc t x _c1_symbol
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c2_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      list → symbol list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_13 ~_loc t a0_list a1_symbol _c0_list = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list a1_symbol () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DTYPE TYPE list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_14 ~_loc t a0_list a1_TYPE _c0_decl = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a13 ~_loc a0_list (Actions.a2 ~_loc (Actions.a0 ~_loc a1_TYPE ()) ()) () () in
      _c0_decl ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DSTART . option list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      option → . TYPE 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      option → . 		/ ID, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    GOTO:
      TYPE -> 3
      option -> 16

    ACTION:
      TYPE -> shift
      ID DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
  *)
  and state_15 ~_loc t _c0_decl = 
    let rec _c1_option ~_loc t x = state_16 ~_loc t x _c0_decl in
    match fst t with
    (* Shift *)
    | TYPE x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_3 ~_loc t x _c1_option
    (* Reduce *)
    | ID _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a3 ~_loc () in
      _c1_option ~_loc t x
    | _ ->
      fail t [ "ID"; "TYPE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DSTART option . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      list → . ID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    GOTO:
      ID -> 17
      list -> 19

    ACTION:
      ID -> shift
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
  *)
  and state_16 ~_loc t a0_option _c0_decl = 
    let rec _c1_list ~_loc t x = state_19 ~_loc t x a0_option _c0_decl in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_17 ~_loc t x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c1_list ~_loc t x
    | _ ->
      fail t [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      list → ID . list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      list → . ID list 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP
      list → . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    GOTO:
      ID -> 17
      list -> 18

    ACTION:
      ID -> shift
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1
  *)
  and state_17 ~_loc t a0_ID _c0_list = 
    let rec _c1_list ~_loc t x = state_18 ~_loc t x a0_ID _c0_list in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_17 ~_loc t x _c1_list
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c1_list ~_loc t x
    | _ ->
      fail t [ "ID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      list → ID list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_18 ~_loc t a0_list a1_ID _c0_list = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list (Actions.a9 ~_loc (Actions.a0 ~_loc a1_ID ()) ()) () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DSTART option list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_19 ~_loc t a0_list a1_option _c0_decl = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a10 ~_loc a0_list a1_option () () in
      _c0_decl ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
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
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
  *)
  and state_20 ~_loc t _c0_decl = 
    let rec _c1_ident ~_loc t x = state_23 ~_loc t x _c2_list
    and _c2_list ~_loc t x = state_25 ~_loc t x _c0_decl in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_21 ~_loc t x _c1_ident
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_22 ~_loc t x _c1_ident
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c2_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      ident → ID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DWHEN

    ACTION:
      ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN -> reduce 0 0
  *)
  and state_21 ~_loc t a0_ID _c0_ident = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DWHEN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a14 ~_loc (Actions.a0 ~_loc a0_ID ()) () in
      _c0_ident ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DWHEN" ]

  (*
    ITEMS:
      ident → TID . 		/ ID, TID, CODE, DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP, DWHEN

    ACTION:
      ID TID CODE DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP DWHEN -> reduce 0 0
  *)
  and state_22 ~_loc t a0_TID _c0_ident = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | DWHEN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a15 ~_loc (Actions.a0 ~_loc a0_TID ()) () in
      _c0_ident ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP"; "DWHEN" ]

  (*
    ITEMS:
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
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
  *)
  and state_23 ~_loc t a0_ident _c0_list = 
    let rec _c1_ident ~_loc t x = state_23 ~_loc t x _c2_list
    and _c2_list ~_loc t x = state_24 ~_loc t x a0_ident _c0_list in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_21 ~_loc t x _c1_ident
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_22 ~_loc t x _c1_ident
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c2_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      list → ident list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_24 ~_loc t a0_list a1_ident _c0_list = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list a1_ident () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DLEFT list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_25 ~_loc t a0_list _c0_decl = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a16 ~_loc a0_list () () in
      _c0_decl ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
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
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
  *)
  and state_26 ~_loc t _c0_decl = 
    let rec _c1_ident ~_loc t x = state_23 ~_loc t x _c2_list
    and _c2_list ~_loc t x = state_27 ~_loc t x _c0_decl in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_21 ~_loc t x _c1_ident
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_22 ~_loc t x _c1_ident
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c2_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DRIGHT list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_27 ~_loc t a0_list _c0_decl = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a17 ~_loc a0_list () () in
      _c0_decl ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
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
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 2 1
  *)
  and state_28 ~_loc t _c0_decl = 
    let rec _c1_ident ~_loc t x = state_23 ~_loc t x _c2_list
    and _c2_list ~_loc t x = state_29 ~_loc t x _c0_decl in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_21 ~_loc t x _c1_ident
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_22 ~_loc t x _c1_ident
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c2_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      decl → DNONASSOC list . 		/ DCODE, DTOKEN, DTYPE, DSTART, DLEFT, DRIGHT, DNONASSOC, DSEP

    ACTION:
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0
  *)
  and state_29 ~_loc t a0_list _c0_decl = 
    match fst t with
    (* Reduce *)
    | DCODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a18 ~_loc a0_list () () in
      _c0_decl ~_loc t x
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
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
      DCODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift
  *)
  and state_30 ~_loc t a0_decl _c0_list = 
    let rec _c1_decl ~_loc t x = state_30 ~_loc t x _c2_list
    and _c2_list ~_loc t x = state_31 ~_loc t x a0_decl _c0_list in
    match fst t with
    (* Reduce *)
    | DSEP ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c2_list ~_loc t x
    (* Shift *)
    | DCODE x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_1 ~_loc t x _c1_decl
    (* Shift *)
    | DTOKEN ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_2 ~_loc t _c1_decl
    (* Shift *)
    | DTYPE ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_8 ~_loc t _c1_decl
    (* Shift *)
    | DSTART ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_15 ~_loc t _c1_decl
    (* Shift *)
    | DLEFT ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_20 ~_loc t _c1_decl
    (* Shift *)
    | DRIGHT ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_26 ~_loc t _c1_decl
    (* Shift *)
    | DNONASSOC ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_28 ~_loc t _c1_decl
    | _ ->
      fail t [ "DCODE"; "DTOKEN"; "DTYPE"; "DSTART"; "DLEFT"; "DRIGHT"; "DNONASSOC"; "DSEP" ]

  (*
    ITEMS:
      list → decl list . 		/ DSEP

    ACTION:
      DSEP -> reduce 0 0
  *)
  and state_31 ~_loc t a0_list a1_decl _c0_list = 
    match fst t with
    (* Reduce *)
    | DSEP ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list a1_decl () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "DSEP" ]

  (*
    ITEMS:
      grammar' → list . DSEP list EOF

    GOTO:
      DSEP -> 33

    ACTION:
      DSEP -> shift
  *)
  and state_32 ~_loc t a0_list _c0_grammar_starting = 
    match fst t with
    (* Shift *)
    | DSEP ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_33 ~_loc t a0_list _c0_grammar_starting
    | _ ->
      fail t [ "DSEP" ]

  (*
    ITEMS:
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
      EOF -> reduce 3 1
  *)
  and state_33 ~_loc t a1_list _c0_grammar_starting = 
    let rec _c1_rule ~_loc t x = state_35 ~_loc t x _c3_list
    and _c2_boption ~_loc t x = state_36 ~_loc t x _c1_rule
    and _c3_list ~_loc t x = state_98 ~_loc t x a1_list _c0_grammar_starting in
    match fst t with
    (* Reduce *)
    | ID _ ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a19 ~_loc () in
      _c2_boption ~_loc t x
    (* Shift *)
    | DINLINE ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_34 ~_loc t _c2_boption
    (* Reduce *)
    | EOF ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c3_list ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "EOF" ]

  (*
    ITEMS:
      boption → DINLINE . 		/ ID

    ACTION:
      ID -> reduce 0 0
  *)
  and state_34 ~_loc t _c0_boption = 
    match fst t with
    (* Reduce *)
    | ID _ ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a20 ~_loc () () in
      _c0_boption ~_loc t x
    | _ ->
      fail t [ "ID" ]

  (*
    ITEMS:
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
      EOF -> reduce 3 1
  *)
  and state_35 ~_loc t a0_rule _c0_list = 
    let rec _c1_rule ~_loc t x = state_35 ~_loc t x _c3_list
    and _c2_boption ~_loc t x = state_36 ~_loc t x _c1_rule
    and _c3_list ~_loc t x = state_97 ~_loc t x a0_rule _c0_list in
    match fst t with
    (* Reduce *)
    | ID _ ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a19 ~_loc () in
      _c2_boption ~_loc t x
    (* Shift *)
    | DINLINE ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_34 ~_loc t _c2_boption
    (* Reduce *)
    | EOF ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c3_list ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "EOF" ]

  (*
    ITEMS:
      rule → boption . ID loption COLON option separated_nonempty_list list 		/ ID, DINLINE, EOF

    GOTO:
      ID -> 37

    ACTION:
      ID -> shift
  *)
  and state_36 ~_loc t a0_boption _c0_rule = 
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_37 ~_loc t x a0_boption _c0_rule
    | _ ->
      fail t [ "ID" ]

  (*
    ITEMS:
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
      COLON -> reduce 2 1
  *)
  and state_37 ~_loc t a0_ID a1_boption _c0_rule = 
    let rec _c1_parameters ~_loc t x = state_45 ~_loc t x _c2_loption
    and _c2_loption ~_loc t x = state_46 ~_loc t x a0_ID a1_boption _c0_rule in
    match fst t with
    (* Shift *)
    | LPAREN ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_38 ~_loc t _c1_parameters
    (* Reduce *)
    | COLON ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a24 ~_loc () in
      _c2_loption ~_loc t x
    | _ ->
      fail t [ "COLON"; "LPAREN" ]

  (*
    ITEMS:
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
      RPAREN -> reduce 3 1
  *)
  and state_38 ~_loc t _c0_parameters = 
    let rec _c1_symbol ~_loc t x = state_39 ~_loc t x _c2_separated_nonempty_list
    and _c2_separated_nonempty_list ~_loc t x = state_42 ~_loc t x _c3_loption
    and _c3_loption ~_loc t x = state_43 ~_loc t x _c0_parameters in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_10 ~_loc t x _c1_symbol
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    (* Reduce *)
    | RPAREN ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a24 ~_loc () in
      _c3_loption ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "RPAREN" ]

  (*
    ITEMS:
      separated_nonempty_list → symbol . COMMA separated_nonempty_list 		/ RPAREN
      separated_nonempty_list → symbol . 		/ RPAREN

    GOTO:
      COMMA -> 40

    ACTION:
      COMMA -> shift
      RPAREN -> reduce 0 1
  *)
  and state_39 ~_loc t a0_symbol _c0_separated_nonempty_list = 
    match fst t with
    (* Shift *)
    | COMMA ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_40 ~_loc t a0_symbol _c0_separated_nonempty_list
    (* Reduce *)
    | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a22 ~_loc (Actions.a21 ~_loc a0_symbol ()) () in
      _c0_separated_nonempty_list ~_loc t x
    | _ ->
      fail t [ "COMMA"; "RPAREN" ]

  (*
    ITEMS:
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
      ID TID -> shift
  *)
  and state_40 ~_loc t a1_symbol _c0_separated_nonempty_list = 
    let rec _c1_symbol ~_loc t x = state_39 ~_loc t x _c2_separated_nonempty_list
    and _c2_separated_nonempty_list ~_loc t x = state_41 ~_loc t x a1_symbol _c0_separated_nonempty_list in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_10 ~_loc t x _c1_symbol
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID" ]

  (*
    ITEMS:
      separated_nonempty_list → symbol COMMA separated_nonempty_list . 		/ RPAREN

    ACTION:
      RPAREN -> reduce 0 0
  *)
  and state_41 ~_loc t a0_separated_nonempty_list a2_symbol _c0_separated_nonempty_list = 
    match fst t with
    (* Reduce *)
    | RPAREN ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a23 ~_loc a0_separated_nonempty_list () (Actions.a21 ~_loc a2_symbol ()) () in
      _c0_separated_nonempty_list ~_loc t x
    | _ ->
      fail t [ "RPAREN" ]

  (*
    ITEMS:
      loption → separated_nonempty_list . 		/ RPAREN

    ACTION:
      RPAREN -> reduce 0 0
  *)
  and state_42 ~_loc t a0_separated_nonempty_list _c0_loption = 
    match fst t with
    (* Reduce *)
    | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a25 ~_loc a0_separated_nonempty_list () in
      _c0_loption ~_loc t x
    | _ ->
      fail t [ "RPAREN" ]

  (*
    ITEMS:
      parameters → LPAREN loption . RPAREN 		/ COLON

    GOTO:
      RPAREN -> 44

    ACTION:
      RPAREN -> shift
  *)
  and state_43 ~_loc t a0_loption _c0_parameters = 
    match fst t with
    (* Shift *)
    | RPAREN ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_44 ~_loc t a0_loption _c0_parameters
    | _ ->
      fail t [ "RPAREN" ]

  (*
    ITEMS:
      parameters → LPAREN loption RPAREN . 		/ COLON

    ACTION:
      COLON -> reduce 0 0
  *)
  and state_44 ~_loc t a1_loption _c0_parameters = 
    match fst t with
    (* Reduce *)
    | COLON ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a27 ~_loc () (Actions.a26 ~_loc a1_loption ()) () () in
      _c0_parameters ~_loc t x
    | _ ->
      fail t [ "COLON" ]

  (*
    ITEMS:
      loption → parameters . 		/ COLON

    ACTION:
      COLON -> reduce 0 0
  *)
  and state_45 ~_loc t a0_parameters _c0_loption = 
    match fst t with
    (* Reduce *)
    | COLON ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a25 ~_loc a0_parameters () in
      _c0_loption ~_loc t x
    | _ ->
      fail t [ "COLON" ]

  (*
    ITEMS:
      rule → boption ID loption . COLON option separated_nonempty_list list 		/ ID, DINLINE, EOF

    GOTO:
      COLON -> 47

    ACTION:
      COLON -> shift
  *)
  and state_46 ~_loc t a0_loption a1_ID a2_boption _c0_rule = 
    match fst t with
    (* Shift *)
    | COLON ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_47 ~_loc t a0_loption a1_ID a2_boption _c0_rule
    | _ ->
      fail t [ "COLON" ]

  (*
    ITEMS:
      rule → boption ID loption COLON . option separated_nonempty_list list 		/ ID, DINLINE, EOF
      option → . BAR 		/ ID, TID, CODE, DWHEN, DPREC
      option → . 		/ ID, TID, CODE, DWHEN, DPREC

    GOTO:
      BAR -> 48
      option -> 49

    ACTION:
      BAR -> shift
      ID TID CODE DWHEN DPREC -> reduce 1 1
  *)
  and state_47 ~_loc t a1_loption a2_ID a3_boption _c0_rule = 
    let rec _c1_option ~_loc t x = state_49 ~_loc t x a1_loption a2_ID a3_boption _c0_rule in
    match fst t with
    (* Shift *)
    | BAR ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_48 ~_loc t _c1_option
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a3 ~_loc () in
      _c1_option ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "BAR" ]

  (*
    ITEMS:
      option → BAR . 		/ ID, TID, CODE, DWHEN, DPREC

    ACTION:
      ID TID CODE DWHEN DPREC -> reduce 0 0
  *)
  and state_48 ~_loc t _c0_option = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a4 ~_loc () () in
      _c0_option ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (*
    ITEMS:
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
      ID TID -> shift
  *)
  and state_49 ~_loc t a0_option a2_loption a3_ID a4_boption _c0_rule = 
    let rec _c1_symbol ~_loc t x = state_52 ~_loc t x _c4_actual
    and _c2_production ~_loc t x = state_76 ~_loc t x _c6_separated_nonempty_list
    and _c3_producer ~_loc t x = state_54 ~_loc t x _c5_list
    and _c4_actual ~_loc t x = state_55 ~_loc t x _c3_producer _c4_actual
    and _c5_list ~_loc t x = state_78 ~_loc t x _c2_production
    and _c6_separated_nonempty_list ~_loc t x = state_95 ~_loc t x a0_option a2_loption a3_ID a4_boption _c0_rule in
    match fst t with
    (* Reduce *)
    | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c5_list ~_loc t x
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_50 ~_loc t x _c1_symbol _c3_producer
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (*
    ITEMS:
      symbol → ID . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, LPAREN, RPAREN
      producer → ID . EQ actual list 		/ ID, TID, CODE, DWHEN, DPREC

    GOTO:
      EQ -> 51

    ACTION:
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR LPAREN RPAREN -> reduce 0 0
      EQ -> shift
  *)
  and state_50 ~_loc t a0_ID _c0_symbol _c1_producer = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | LPAREN | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a11 ~_loc a0_ID () in
      _c0_symbol ~_loc t x
    (* Shift *)
    | EQ ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_51 ~_loc t a0_ID _c1_producer
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "EQ"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "LPAREN"; "RPAREN" ]

  (*
    ITEMS:
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
      ID TID -> shift
  *)
  and state_51 ~_loc t a1_ID _c0_producer = 
    let rec _c1_symbol ~_loc t x = state_52 ~_loc t x _c2_actual
    and _c2_actual ~_loc t x = state_74 ~_loc t x a1_ID _c0_producer _c2_actual in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_10 ~_loc t x _c1_symbol
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID" ]

  (*
    ITEMS:
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
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 2 1
  *)
  and state_52 ~_loc t a0_symbol _c0_actual = 
    let rec _c1_args ~_loc t x = state_72 ~_loc t x _c2_loption
    and _c2_loption ~_loc t x = state_73 ~_loc t x a0_symbol _c0_actual in
    match fst t with
    (* Shift *)
    | LPAREN ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_53 ~_loc t _c1_args
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a24 ~_loc () in
      _c2_loption ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "LPAREN"; "RPAREN" ]

  (*
    ITEMS:
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
      ID TID -> shift
  *)
  and state_53 ~_loc t _c0_args = 
    let rec _c1_symbol ~_loc t x = state_52 ~_loc t x _c3_actual
    and _c2_producer ~_loc t x = state_54 ~_loc t x _c5_list
    and _c3_actual ~_loc t x = state_64 ~_loc t x _c2_producer _c3_actual _c4_arg
    and _c4_arg ~_loc t x = state_65 ~_loc t x _c6_separated_nonempty_list
    and _c5_list ~_loc t x = state_67 ~_loc t x _c4_arg
    and _c6_separated_nonempty_list ~_loc t x = state_70 ~_loc t x _c0_args in
    match fst t with
    (* Reduce *)
    | CODE _ ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c5_list ~_loc t x
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_50 ~_loc t x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID"; "CODE" ]

  (*
    ITEMS:
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
      ID TID -> shift
  *)
  and state_54 ~_loc t a0_producer _c0_list = 
    let rec _c1_symbol ~_loc t x = state_52 ~_loc t x _c3_actual
    and _c2_producer ~_loc t x = state_54 ~_loc t x _c4_list
    and _c3_actual ~_loc t x = state_55 ~_loc t x _c2_producer _c3_actual
    and _c4_list ~_loc t x = state_63 ~_loc t x a0_producer _c0_list in
    match fst t with
    (* Reduce *)
    | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c4_list ~_loc t x
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_50 ~_loc t x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (*
    ITEMS:
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
      ID TID CODE DWHEN DPREC -> reduce 3 1
  *)
  and state_55 ~_loc t a0_actual _c0_producer _c1_actual = 
    let rec _c2_shorthand ~_loc t x = state_61 ~_loc t x a0_actual _c1_actual
    and _c3_list ~_loc t x = state_62 ~_loc t x a0_actual _c0_producer in
    match fst t with
    (* Shift *)
    | PLUS ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_56 ~_loc t _c2_shorthand
    (* Shift *)
    | QMARK ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_57 ~_loc t _c2_shorthand
    (* Shift *)
    | SEMI ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_58 ~_loc t _c3_list
    (* Shift *)
    | STAR ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_60 ~_loc t _c2_shorthand
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c3_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "PLUS"; "QMARK"; "SEMI"; "STAR" ]

  (*
    ITEMS:
      shorthand → PLUS . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN

    ACTION:
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0
  *)
  and state_56 ~_loc t _c0_shorthand = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a31 ~_loc () () in
      _c0_shorthand ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (*
    ITEMS:
      shorthand → QMARK . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN

    ACTION:
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0
  *)
  and state_57 ~_loc t _c0_shorthand = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a33 ~_loc () () in
      _c0_shorthand ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (*
    ITEMS:
      list → SEMI . list 		/ ID, TID, CODE, DWHEN, DINLINE, DPREC, EOF
      list → . SEMI list 		/ ID, TID, CODE, DWHEN, DINLINE, DPREC, EOF
      list → . 		/ ID, TID, CODE, DWHEN, DINLINE, DPREC, EOF

    GOTO:
      SEMI -> 58
      list -> 59

    ACTION:
      SEMI -> shift
      ID TID CODE DWHEN DINLINE DPREC EOF -> reduce 1 1
  *)
  and state_58 ~_loc t _c0_list = 
    let rec _c1_list ~_loc t x = state_59 ~_loc t x _c0_list in
    match fst t with
    (* Shift *)
    | SEMI ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_58 ~_loc t _c1_list
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DINLINE | DPREC | EOF ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c1_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DINLINE"; "DPREC"; "SEMI"; "EOF" ]

  (*
    ITEMS:
      list → SEMI list . 		/ ID, TID, CODE, DWHEN, DINLINE, DPREC, EOF

    ACTION:
      ID TID CODE DWHEN DINLINE DPREC EOF -> reduce 0 0
  *)
  and state_59 ~_loc t a0_list _c0_list = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DINLINE | DPREC | EOF ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list () () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DINLINE"; "DPREC"; "EOF" ]

  (*
    ITEMS:
      shorthand → STAR . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN

    ACTION:
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0
  *)
  and state_60 ~_loc t _c0_shorthand = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a32 ~_loc () () in
      _c0_shorthand ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (*
    ITEMS:
      actual → actual shorthand . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN

    ACTION:
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0
  *)
  and state_61 ~_loc t a0_shorthand a1_actual _c0_actual = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a34 ~_loc a0_shorthand a1_actual () in
      _c0_actual ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (*
    ITEMS:
      producer → actual list . 		/ ID, TID, CODE, DWHEN, DPREC

    ACTION:
      ID TID CODE DWHEN DPREC -> reduce 0 0
  *)
  and state_62 ~_loc t a0_list a1_actual _c0_producer = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a40 ~_loc a0_list a1_actual (Actions.a29 ~_loc ()) () in
      _c0_producer ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (*
    ITEMS:
      list → producer list . 		/ CODE, DWHEN, DPREC

    ACTION:
      CODE DWHEN DPREC -> reduce 0 0
  *)
  and state_63 ~_loc t a0_list a1_producer _c0_list = 
    match fst t with
    (* Reduce *)
    | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list a1_producer () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "CODE"; "DWHEN"; "DPREC" ]

  (*
    ITEMS:
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
      COMMA RPAREN -> reduce 2 0
  *)
  and state_64 ~_loc t a0_actual _c0_producer _c1_actual _c2_arg = 
    let rec _c3_shorthand ~_loc t x = state_61 ~_loc t x a0_actual _c1_actual
    and _c4_list ~_loc t x = state_62 ~_loc t x a0_actual _c0_producer in
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c4_list ~_loc t x
    (* Shift *)
    | PLUS ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_56 ~_loc t _c3_shorthand
    (* Shift *)
    | QMARK ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_57 ~_loc t _c3_shorthand
    (* Shift *)
    | SEMI ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_58 ~_loc t _c4_list
    (* Shift *)
    | STAR ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_60 ~_loc t _c3_shorthand
    (* Reduce *)
    | COMMA | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a35 ~_loc a0_actual () in
      _c2_arg ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (*
    ITEMS:
      separated_nonempty_list → arg . COMMA separated_nonempty_list 		/ RPAREN
      separated_nonempty_list → arg . 		/ RPAREN

    GOTO:
      COMMA -> 66

    ACTION:
      COMMA -> shift
      RPAREN -> reduce 0 1
  *)
  and state_65 ~_loc t a0_arg _c0_separated_nonempty_list = 
    match fst t with
    (* Shift *)
    | COMMA ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_66 ~_loc t a0_arg _c0_separated_nonempty_list
    (* Reduce *)
    | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a22 ~_loc a0_arg () in
      _c0_separated_nonempty_list ~_loc t x
    | _ ->
      fail t [ "COMMA"; "RPAREN" ]

  (*
    ITEMS:
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
      ID TID -> shift
  *)
  and state_66 ~_loc t a1_arg _c0_separated_nonempty_list = 
    let rec _c1_symbol ~_loc t x = state_52 ~_loc t x _c3_actual
    and _c2_producer ~_loc t x = state_54 ~_loc t x _c5_list
    and _c3_actual ~_loc t x = state_64 ~_loc t x _c2_producer _c3_actual _c4_arg
    and _c4_arg ~_loc t x = state_65 ~_loc t x _c6_separated_nonempty_list
    and _c5_list ~_loc t x = state_67 ~_loc t x _c4_arg
    and _c6_separated_nonempty_list ~_loc t x = state_69 ~_loc t x a1_arg _c0_separated_nonempty_list in
    match fst t with
    (* Reduce *)
    | CODE _ ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c5_list ~_loc t x
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_50 ~_loc t x _c1_symbol _c2_producer
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID"; "CODE" ]

  (*
    ITEMS:
      arg → list . CODE 		/ COMMA, RPAREN

    GOTO:
      CODE -> 68

    ACTION:
      CODE -> shift
  *)
  and state_67 ~_loc t a0_list _c0_arg = 
    match fst t with
    (* Shift *)
    | CODE x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_68 ~_loc t x a0_list _c0_arg
    | _ ->
      fail t [ "CODE" ]

  (*
    ITEMS:
      arg → list CODE . 		/ COMMA, RPAREN

    ACTION:
      COMMA RPAREN -> reduce 0 0
  *)
  and state_68 ~_loc t a0_CODE a1_list _c0_arg = 
    match fst t with
    (* Reduce *)
    | COMMA | RPAREN ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a37 ~_loc (Actions.a36 ~_loc (Actions.a0 ~_loc a0_CODE ()) ()) a1_list () in
      _c0_arg ~_loc t x
    | _ ->
      fail t [ "COMMA"; "RPAREN" ]

  (*
    ITEMS:
      separated_nonempty_list → arg COMMA separated_nonempty_list . 		/ RPAREN

    ACTION:
      RPAREN -> reduce 0 0
  *)
  and state_69 ~_loc t a0_separated_nonempty_list a2_arg _c0_separated_nonempty_list = 
    match fst t with
    (* Reduce *)
    | RPAREN ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a23 ~_loc a0_separated_nonempty_list () a2_arg () in
      _c0_separated_nonempty_list ~_loc t x
    | _ ->
      fail t [ "RPAREN" ]

  (*
    ITEMS:
      args → LPAREN separated_nonempty_list . RPAREN 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN

    GOTO:
      RPAREN -> 71

    ACTION:
      RPAREN -> shift
  *)
  and state_70 ~_loc t a0_separated_nonempty_list _c0_args = 
    match fst t with
    (* Shift *)
    | RPAREN ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_71 ~_loc t a0_separated_nonempty_list _c0_args
    | _ ->
      fail t [ "RPAREN" ]

  (*
    ITEMS:
      args → LPAREN separated_nonempty_list RPAREN . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN

    ACTION:
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0
  *)
  and state_71 ~_loc t a1_separated_nonempty_list _c0_args = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a38 ~_loc () a1_separated_nonempty_list () () in
      _c0_args ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (*
    ITEMS:
      loption → args . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN

    ACTION:
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0
  *)
  and state_72 ~_loc t a0_args _c0_loption = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a25 ~_loc a0_args () in
      _c0_loption ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (*
    ITEMS:
      actual → symbol loption . 		/ ID, TID, CODE, DWHEN, DPREC, COMMA, PLUS, QMARK, SEMI, STAR, RPAREN

    ACTION:
      ID TID CODE DWHEN DPREC COMMA PLUS QMARK SEMI STAR RPAREN -> reduce 0 0
  *)
  and state_73 ~_loc t a0_loption a1_symbol _c0_actual = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC | COMMA | PLUS | QMARK | SEMI | STAR | RPAREN ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a39 ~_loc a0_loption a1_symbol () in
      _c0_actual ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "COMMA"; "PLUS"; "QMARK"; "SEMI"; "STAR"; "RPAREN" ]

  (*
    ITEMS:
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
      ID TID CODE DWHEN DPREC -> reduce 3 1
  *)
  and state_74 ~_loc t a0_actual a2_ID _c0_producer _c1_actual = 
    let rec _c2_shorthand ~_loc t x = state_61 ~_loc t x a0_actual _c1_actual
    and _c3_list ~_loc t x = state_75 ~_loc t x a0_actual a2_ID _c0_producer in
    match fst t with
    (* Shift *)
    | PLUS ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_56 ~_loc t _c2_shorthand
    (* Shift *)
    | QMARK ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_57 ~_loc t _c2_shorthand
    (* Shift *)
    | SEMI ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_58 ~_loc t _c3_list
    (* Shift *)
    | STAR ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_60 ~_loc t _c2_shorthand
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c3_list ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC"; "PLUS"; "QMARK"; "SEMI"; "STAR" ]

  (*
    ITEMS:
      producer → ID EQ actual list . 		/ ID, TID, CODE, DWHEN, DPREC

    ACTION:
      ID TID CODE DWHEN DPREC -> reduce 0 0
  *)
  and state_75 ~_loc t a0_list a1_actual a3_ID _c0_producer = 
    match fst t with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 4
      and x = Actions.a40 ~_loc a0_list a1_actual (Actions.a30 ~_loc (Actions.a28 ~_loc () (Actions.a9 ~_loc (Actions.a0 ~_loc a3_ID ()) ()) ()) ()) () in
      _c0_producer ~_loc t x
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (*
    ITEMS:
      separated_nonempty_list → production . BAR separated_nonempty_list 		/ ID, DINLINE, SEMI, EOF
      separated_nonempty_list → production . 		/ ID, DINLINE, SEMI, EOF

    GOTO:
      BAR -> 77

    ACTION:
      BAR -> shift
      ID DINLINE SEMI EOF -> reduce 0 1
  *)
  and state_76 ~_loc t a0_production _c0_separated_nonempty_list = 
    match fst t with
    (* Shift *)
    | BAR ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_77 ~_loc t a0_production _c0_separated_nonempty_list
    (* Reduce *)
    | ID _ | DINLINE | SEMI | EOF ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a22 ~_loc a0_production () in
      _c0_separated_nonempty_list ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (*
    ITEMS:
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
      ID TID -> shift
  *)
  and state_77 ~_loc t a1_production _c0_separated_nonempty_list = 
    let rec _c1_symbol ~_loc t x = state_52 ~_loc t x _c4_actual
    and _c2_production ~_loc t x = state_76 ~_loc t x _c6_separated_nonempty_list
    and _c3_producer ~_loc t x = state_54 ~_loc t x _c5_list
    and _c4_actual ~_loc t x = state_55 ~_loc t x _c3_producer _c4_actual
    and _c5_list ~_loc t x = state_78 ~_loc t x _c2_production
    and _c6_separated_nonempty_list ~_loc t x = state_94 ~_loc t x a1_production _c0_separated_nonempty_list in
    match fst t with
    (* Reduce *)
    | CODE _ | DWHEN | DPREC ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c5_list ~_loc t x
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_50 ~_loc t x _c1_symbol _c3_producer
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID"; "CODE"; "DWHEN"; "DPREC" ]

  (*
    ITEMS:
      production → list . option actions 		/ ID, DINLINE, BAR, SEMI, EOF
      option → . DPREC ident 		/ CODE, DWHEN
      option → . 		/ CODE, DWHEN

    GOTO:
      DPREC -> 79
      option -> 81

    ACTION:
      DPREC -> shift
      CODE DWHEN -> reduce 1 1
  *)
  and state_78 ~_loc t a0_list _c0_production = 
    let rec _c1_option ~_loc t x = state_81 ~_loc t x a0_list _c0_production in
    match fst t with
    (* Shift *)
    | DPREC ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_79 ~_loc t _c1_option
    (* Reduce *)
    | CODE _ | DWHEN ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a3 ~_loc () in
      _c1_option ~_loc t x
    | _ ->
      fail t [ "CODE"; "DWHEN"; "DPREC" ]

  (*
    ITEMS:
      option → DPREC . ident 		/ CODE, DWHEN
      ident → . ID 		/ CODE, DWHEN
      ident → . TID 		/ CODE, DWHEN

    GOTO:
      ID -> 21
      TID -> 22
      ident -> 80

    ACTION:
      ID TID -> shift
  *)
  and state_79 ~_loc t _c0_option = 
    let rec _c1_ident ~_loc t x = state_80 ~_loc t x _c0_option in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_21 ~_loc t x _c1_ident
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_22 ~_loc t x _c1_ident
    | _ ->
      fail t [ "ID"; "TID" ]

  (*
    ITEMS:
      option → DPREC ident . 		/ CODE, DWHEN

    ACTION:
      CODE DWHEN -> reduce 0 0
  *)
  and state_80 ~_loc t a0_ident _c0_option = 
    match fst t with
    (* Reduce *)
    | CODE _ | DWHEN ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a4 ~_loc (Actions.a41 ~_loc a0_ident () ()) () in
      _c0_option ~_loc t x
    | _ ->
      fail t [ "CODE"; "DWHEN" ]

  (*
    ITEMS:
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
      DWHEN -> shift
  *)
  and state_81 ~_loc t a0_option a1_list _c0_production = 
    let rec _c1_actions ~_loc t x = state_87 ~_loc t x a0_option a1_list _c0_production
    and _c2_conditional_action ~_loc t x = state_88 ~_loc t x _c3_nonempty_list _c4_list
    and _c3_nonempty_list ~_loc t x = state_91 ~_loc t x _c1_actions
    and _c4_list ~_loc t x = state_92 ~_loc t x _c1_actions in
    match fst t with
    (* Reduce *)
    | CODE _ ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c4_list ~_loc t x
    (* Shift *)
    | DWHEN ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_82 ~_loc t _c2_conditional_action
    | _ ->
      fail t [ "CODE"; "DWHEN" ]

  (*
    ITEMS:
      conditional_action → DWHEN . symbol EQ symbol CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
      symbol → . ID 		/ EQ
      symbol → . TID 		/ EQ

    GOTO:
      ID -> 10
      TID -> 11
      symbol -> 83

    ACTION:
      ID TID -> shift
  *)
  and state_82 ~_loc t _c0_conditional_action = 
    let rec _c1_symbol ~_loc t x = state_83 ~_loc t x _c0_conditional_action in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_10 ~_loc t x _c1_symbol
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID" ]

  (*
    ITEMS:
      conditional_action → DWHEN symbol . EQ symbol CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF

    GOTO:
      EQ -> 84

    ACTION:
      EQ -> shift
  *)
  and state_83 ~_loc t a0_symbol _c0_conditional_action = 
    match fst t with
    (* Shift *)
    | EQ ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_84 ~_loc t a0_symbol _c0_conditional_action
    | _ ->
      fail t [ "EQ" ]

  (*
    ITEMS:
      conditional_action → DWHEN symbol EQ . symbol CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF
      symbol → . ID 		/ CODE
      symbol → . TID 		/ CODE

    GOTO:
      ID -> 10
      TID -> 11
      symbol -> 85

    ACTION:
      ID TID -> shift
  *)
  and state_84 ~_loc t a1_symbol _c0_conditional_action = 
    let rec _c1_symbol ~_loc t x = state_85 ~_loc t x a1_symbol _c0_conditional_action in
    match fst t with
    (* Shift *)
    | ID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_10 ~_loc t x _c1_symbol
    (* Shift *)
    | TID x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_11 ~_loc t x _c1_symbol
    | _ ->
      fail t [ "ID"; "TID" ]

  (*
    ITEMS:
      conditional_action → DWHEN symbol EQ symbol . CODE 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF

    GOTO:
      CODE -> 86

    ACTION:
      CODE -> shift
  *)
  and state_85 ~_loc t a0_symbol a2_symbol _c0_conditional_action = 
    match fst t with
    (* Shift *)
    | CODE x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_86 ~_loc t x a0_symbol a2_symbol _c0_conditional_action
    | _ ->
      fail t [ "CODE" ]

  (*
    ITEMS:
      conditional_action → DWHEN symbol EQ symbol CODE . 		/ ID, CODE, DWHEN, DINLINE, BAR, SEMI, EOF

    ACTION:
      ID CODE DWHEN DINLINE BAR SEMI EOF -> reduce 0 0
  *)
  and state_86 ~_loc t a0_CODE a1_symbol a3_symbol _c0_conditional_action = 
    match fst t with
    (* Reduce *)
    | ID _ | CODE _ | DWHEN | DINLINE | BAR | SEMI | EOF ->
      let _loc = loc_reduce ~_loc 5
      and x = Actions.a42 ~_loc (Actions.a36 ~_loc (Actions.a0 ~_loc a0_CODE ()) ()) a1_symbol () a3_symbol () () in
      _c0_conditional_action ~_loc t x
    | _ ->
      fail t [ "ID"; "CODE"; "DWHEN"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (*
    ITEMS:
      production → list option actions . 		/ ID, DINLINE, BAR, SEMI, EOF

    ACTION:
      ID DINLINE BAR SEMI EOF -> reduce 0 0
  *)
  and state_87 ~_loc t a0_actions a1_option a2_list _c0_production = 
    match fst t with
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a47 ~_loc a0_actions a1_option a2_list () in
      _c0_production ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (*
    ITEMS:
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
      ID DINLINE BAR SEMI EOF -> reduce 0 1
  *)
  and state_88 ~_loc t a0_conditional_action _c0_nonempty_list _c1_list = 
    let rec _c2_conditional_action ~_loc t x = state_88 ~_loc t x _c3_nonempty_list _c4_list
    and _c3_nonempty_list ~_loc t x = state_89 ~_loc t x a0_conditional_action _c0_nonempty_list
    and _c4_list ~_loc t x = state_90 ~_loc t x a0_conditional_action _c1_list in
    match fst t with
    (* Reduce *)
    | CODE _ ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c4_list ~_loc t x
    (* Shift *)
    | DWHEN ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_82 ~_loc t _c2_conditional_action
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a43 ~_loc a0_conditional_action () in
      _c0_nonempty_list ~_loc t x
    | _ ->
      fail t [ "ID"; "CODE"; "DWHEN"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (*
    ITEMS:
      nonempty_list → conditional_action nonempty_list . 		/ ID, DINLINE, BAR, SEMI, EOF

    ACTION:
      ID DINLINE BAR SEMI EOF -> reduce 0 0
  *)
  and state_89 ~_loc t a0_nonempty_list a1_conditional_action _c0_nonempty_list = 
    match fst t with
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a44 ~_loc a0_nonempty_list a1_conditional_action () in
      _c0_nonempty_list ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (*
    ITEMS:
      list → conditional_action list . 		/ CODE

    ACTION:
      CODE -> reduce 0 0
  *)
  and state_90 ~_loc t a0_list a1_conditional_action _c0_list = 
    match fst t with
    (* Reduce *)
    | CODE _ ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list a1_conditional_action () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "CODE" ]

  (*
    ITEMS:
      actions → nonempty_list . 		/ ID, DINLINE, BAR, SEMI, EOF

    ACTION:
      ID DINLINE BAR SEMI EOF -> reduce 0 0
  *)
  and state_91 ~_loc t a0_nonempty_list _c0_actions = 
    match fst t with
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let _loc = loc_reduce ~_loc 1
      and x = Actions.a45 ~_loc a0_nonempty_list () in
      _c0_actions ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (*
    ITEMS:
      actions → list . CODE 		/ ID, DINLINE, BAR, SEMI, EOF

    GOTO:
      CODE -> 93

    ACTION:
      CODE -> shift
  *)
  and state_92 ~_loc t a0_list _c0_actions = 
    match fst t with
    (* Shift *)
    | CODE x ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_93 ~_loc t x a0_list _c0_actions
    | _ ->
      fail t [ "CODE" ]

  (*
    ITEMS:
      actions → list CODE . 		/ ID, DINLINE, BAR, SEMI, EOF

    ACTION:
      ID DINLINE BAR SEMI EOF -> reduce 0 0
  *)
  and state_93 ~_loc t a0_CODE a1_list _c0_actions = 
    match fst t with
    (* Reduce *)
    | ID _ | DINLINE | BAR | SEMI | EOF ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a46 ~_loc (Actions.a36 ~_loc (Actions.a0 ~_loc a0_CODE ()) ()) a1_list () in
      _c0_actions ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "BAR"; "SEMI"; "EOF" ]

  (*
    ITEMS:
      separated_nonempty_list → production BAR separated_nonempty_list . 		/ ID, DINLINE, SEMI, EOF

    ACTION:
      ID DINLINE SEMI EOF -> reduce 0 0
  *)
  and state_94 ~_loc t a0_separated_nonempty_list a2_production _c0_separated_nonempty_list = 
    match fst t with
    (* Reduce *)
    | ID _ | DINLINE | SEMI | EOF ->
      let _loc = loc_reduce ~_loc 3
      and x = Actions.a23 ~_loc a0_separated_nonempty_list () a2_production () in
      _c0_separated_nonempty_list ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "SEMI"; "EOF" ]

  (*
    ITEMS:
      rule → boption ID loption COLON option separated_nonempty_list . list 		/ ID, DINLINE, EOF
      list → . SEMI list 		/ ID, DINLINE, EOF
      list → . 		/ ID, DINLINE, EOF

    GOTO:
      SEMI -> 58
      list -> 96

    ACTION:
      SEMI -> shift
      ID DINLINE EOF -> reduce 1 1
  *)
  and state_95 ~_loc t a0_separated_nonempty_list a1_option a3_loption a4_ID a5_boption _c0_rule = 
    let rec _c1_list ~_loc t x = state_96 ~_loc t x a0_separated_nonempty_list a1_option a3_loption a4_ID a5_boption _c0_rule in
    match fst t with
    (* Shift *)
    | SEMI ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_58 ~_loc t _c1_list
    (* Reduce *)
    | ID _ | DINLINE | EOF ->
      let _loc = loc_reduce ~_loc 0
      and x = Actions.a6 ~_loc () in
      _c1_list ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "SEMI"; "EOF" ]

  (*
    ITEMS:
      rule → boption ID loption COLON option separated_nonempty_list list . 		/ ID, DINLINE, EOF

    ACTION:
      ID DINLINE EOF -> reduce 0 0
  *)
  and state_96 ~_loc t a0_list a1_separated_nonempty_list a2_option a4_loption a5_ID a6_boption _c0_rule = 
    match fst t with
    (* Reduce *)
    | ID _ | DINLINE | EOF ->
      let _loc = loc_reduce ~_loc 7
      and x = Actions.a48 ~_loc a0_list a1_separated_nonempty_list a2_option () a4_loption (Actions.a9 ~_loc (Actions.a0 ~_loc a5_ID ()) ()) a6_boption () in
      _c0_rule ~_loc t x
    | _ ->
      fail t [ "ID"; "DINLINE"; "EOF" ]

  (*
    ITEMS:
      list → rule list . 		/ EOF

    ACTION:
      EOF -> reduce 0 0
  *)
  and state_97 ~_loc t a0_list a1_rule _c0_list = 
    match fst t with
    (* Reduce *)
    | EOF ->
      let _loc = loc_reduce ~_loc 2
      and x = Actions.a7 ~_loc a0_list a1_rule () in
      _c0_list ~_loc t x
    | _ ->
      fail t [ "EOF" ]

  (*
    ITEMS:
      grammar' → list DSEP list . EOF

    GOTO:
      EOF -> 99

    ACTION:
      EOF -> shift
  *)
  and state_98 ~_loc t a0_list a2_list _c0_grammar_starting = 
    match fst t with
    (* Shift *)
    | EOF ->
      let _loc = loc_shift ~_loc (snd t) in
      let t = shift () in
      state_99 ~_loc t a0_list a2_list _c0_grammar_starting
    | _ ->
      fail t [ "EOF" ]

  (*
    ITEMS:
      grammar' → list DSEP list EOF .
  *)
  and state_99 ~_loc t a1_list a3_list _c0_grammar_starting = 
    let x = Actions.a49 ~_loc () a1_list () a3_list () in
    _c0_grammar_starting t x
  ;;
end

let error_token () = !States.error_token
let expected_tokens () = !States.expected_tokens

let grammar lexfun lexbuf = 
  States.setup lexfun lexbuf;
  let _loc = [] in
  let t = States.shift () in
  States.state_0 ~_loc t (fun _ x -> x)
;;
