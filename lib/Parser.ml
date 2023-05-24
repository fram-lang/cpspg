[@@@warning "-unused-rec-flag"]

open Grammar

let mknode ~loc data = { loc; data }

type token =
  | ID of (string)
  | TID of (string)
  | TYPE of (string)
  | CODE of (string)
  | DTOKEN
  | DTYPE
  | DSTART
  | DLEFT
  | DRIGHT
  | DNONASSOC
  | DPREC
  | DSEP
  | COLON
  | SEMI
  | BAR
  | EQ
  | EOF

module Actions = struct
  let a0_grammar ~loc:_loc rules decls header () = { header; decls; rules }
  let a1_decls ~loc:_loc xs x () = x :: xs
  let a2_decls ~loc:_loc () = []
  let a3_decl ~loc:_loc xs tp () = DeclToken (Some tp, xs)
  let a4_decl ~loc:_loc xs tp () = DeclStart (Some tp, xs)
  let a5_decl ~loc:_loc xs tp () = DeclType (tp, xs)
  let a6_decl ~loc:_loc xs () = DeclToken (None, xs)
  let a7_decl ~loc:_loc xs () = DeclStart (None, xs)
  let a8_decl ~loc:_loc xs () = DeclLeft xs
  let a9_decl ~loc:_loc xs () = DeclRight xs
  let a10_decl ~loc:_loc xs () = DeclNonassoc xs
  let a11_rules ~loc:_loc xs x () = x :: xs
  let a12_rules ~loc:_loc () = []
  let a13_rule ~loc:_loc prods id () = { id; prods }
  let a14_rule_prods ~loc:_loc xs x () = x :: xs
  let a15_rule_prods ~loc:_loc xs () = xs
  let a16_productions ~loc:_loc xs x () = x :: xs
  let a17_productions ~loc:_loc () = []
  let a18_production ~loc:_loc action prec prod () = { prod; prec; action }
  let a19_production_prec ~loc:_loc x () = Some x
  let a20_production_prec ~loc:_loc () = None
  let a21_producers ~loc:_loc xs x () = x :: xs
  let a22_producers ~loc:_loc () = []
  let a23_producer ~loc:_loc actual id () = { id = Some id; actual }
  let a24_producer ~loc:_loc actual () = { id = None; actual }
  let a25_ids ~loc:_loc xs x () = x :: xs
  let a26_ids ~loc:_loc () = []
  let a27_tids ~loc:_loc xs x () = x :: xs
  let a28_tids ~loc:_loc () = []
  let a29_symbols ~loc:_loc xs x () = x :: xs
  let a30_symbols ~loc:_loc () = []
  let a31_symbol ~loc:_loc name () = NTerm name
  let a32_symbol ~loc:_loc name () = Term name
  let a33_id ~loc:_loc x () = mknode ~loc:(List.hd _loc) x
  let a34_tid ~loc:_loc x () = mknode ~loc:(List.hd _loc) x
  let a35_tp ~loc:_loc x () = mknode ~loc:(List.hd _loc) x
  let a36_code ~loc:_loc x () = mknode ~loc:(List.hd _loc) x
end

module States = struct
  let lexfun = ref (fun _ -> assert false)
  let lexbuf = ref (Lexing.from_string String.empty)
  let peeked = ref None
  let lexbuf_fallback_p = ref Lexing.dummy_pos

  let setup lf lb =
    lexfun := lf;
    lexbuf := lb;
    peeked := None;
    lexbuf_fallback_p := !lexbuf.lex_curr_p
  ;;

  let shift () =
    let loc, _ = Option.get !peeked in
    peeked := None;
    lexbuf_fallback_p := !lexbuf.lex_curr_p;
    loc
  ;;

  let lookahead () =
    match !peeked with
    | Some (_, tok) -> tok
    | None ->
      let tok = !lexfun !lexbuf
      and loc = !lexbuf.lex_start_p, !lexbuf.lex_curr_p in
      peeked := Some (loc, tok);
      tok
  ;;

  let reduce_loc ~loc = function
    | 0 -> (!lexbuf_fallback_p, !lexbuf_fallback_p) :: loc
    | n ->
      let rec skip n xs = if n = 0 then xs else skip (n - 1) (List.tl xs) in
      let l = fst (List.nth loc (n - 1)), snd (List.hd loc) in
      l :: skip n loc
  ;;

  (* ITEMS:
       grammar' → . code decls DSEP rules EOF
       code → . CODE /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       CODE -> 1
       code -> 2
     ACTION:
       CODE -> shift *)
  let rec state_0 ~loc _c0_grammar_starting =
    let rec _c1_code ~loc x = state_2 ~loc x _c0_grammar_starting in
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let loc = shift () :: loc in
      state_1 ~loc x _c1_code
    | _ -> raise (Failure "error in state 0")

  (* ITEMS:
       code → CODE . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP /SEMI /BAR
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP SEMI BAR -> reduce 0 0 *)
  and state_1 ~loc a0_CODE _c0_code =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP | SEMI | BAR ->
      let x = Actions.a36_code ~loc a0_CODE ()
      and loc = reduce_loc ~loc 1 in
      _c0_code ~loc x
    | _ -> raise (Failure "error in state 1")

  (* ITEMS:
       grammar' → code . decls DSEP rules EOF
       decls → . decl decls /DSEP
       decls → . /DSEP
       decl → . DTOKEN tp tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DSTART tp ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DTYPE tp symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DTOKEN tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DSTART ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DLEFT symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DRIGHT symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DNONASSOC symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       DTOKEN -> 3
       DTYPE -> 11
       DSTART -> 19
       DLEFT -> 25
       DRIGHT -> 27
       DNONASSOC -> 29
       decls -> 31
       decl -> 58
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift
       DSEP -> reduce 1 1 *)
  and state_2 ~loc a0_code _c0_grammar_starting =
    let rec _c1_decls ~loc x = state_31 ~loc x a0_code _c0_grammar_starting
    and _c2_decl ~loc x = state_58 ~loc x _c1_decls in
    match lookahead () with
    (* Shift *)
    | DTOKEN ->
      let loc = shift () :: loc in
      state_3 ~loc _c2_decl
    (* Shift *)
    | DTYPE ->
      let loc = shift () :: loc in
      state_11 ~loc _c2_decl
    (* Shift *)
    | DSTART ->
      let loc = shift () :: loc in
      state_19 ~loc _c2_decl
    (* Shift *)
    | DLEFT ->
      let loc = shift () :: loc in
      state_25 ~loc _c2_decl
    (* Shift *)
    | DRIGHT ->
      let loc = shift () :: loc in
      state_27 ~loc _c2_decl
    (* Shift *)
    | DNONASSOC ->
      let loc = shift () :: loc in
      state_29 ~loc _c2_decl
    (* Reduce *)
    | DSEP ->
      let x = Actions.a2_decls ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_decls ~loc x
    | _ -> raise (Failure "error in state 2")

  (* ITEMS:
       decl → DTOKEN . tp tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → DTOKEN . tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . tid tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tid → . TID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tp → . TYPE /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       TID -> 4
       TYPE -> 5
       tids -> 6
       tid -> 7
       tp -> 9
     ACTION:
       TID TYPE -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_3 ~loc _c0_decl =
    let rec _c1_tids ~loc x = state_6 ~loc x _c0_decl
    and _c2_tid ~loc x = state_7 ~loc x _c1_tids
    and _c3_tp ~loc x = state_9 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c2_tid
    (* Shift *)
    | TYPE x ->
      let loc = shift () :: loc in
      state_5 ~loc x _c3_tp
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a28_tids ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_tids ~loc x
    | _ -> raise (Failure "error in state 3")

  (* ITEMS:
       tid → TID . /ID /TID /CODE /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DPREC /DSEP
     GOTO:
       
     ACTION:
       ID TID CODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP -> reduce 0 0 *)
  and state_4 ~loc a0_TID _c0_tid =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DPREC | DSEP ->
      let x = Actions.a34_tid ~loc a0_TID ()
      and loc = reduce_loc ~loc 1 in
      _c0_tid ~loc x
    | _ -> raise (Failure "error in state 4")

  (* ITEMS:
       tp → TYPE . /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       ID TID DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_5 ~loc a0_TYPE _c0_tp =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a35_tp ~loc a0_TYPE ()
      and loc = reduce_loc ~loc 1 in
      _c0_tp ~loc x
    | _ -> raise (Failure "error in state 5")

  (* ITEMS:
       decl → DTOKEN tids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_6 ~loc a0_tids _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a6_decl ~loc a0_tids ()
      and loc = reduce_loc ~loc 2 in
      _c0_decl ~loc x
    | _ -> raise (Failure "error in state 6")

  (* ITEMS:
       tids → tid . tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . tid tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tid → . TID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       TID -> 4
       tids -> 8
       tid -> 7
     ACTION:
       TID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_7 ~loc a0_tid _c0_tids =
    let rec _c1_tids ~loc x = state_8 ~loc x a0_tid _c0_tids
    and _c2_tid ~loc x = state_7 ~loc x _c1_tids in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c2_tid
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a28_tids ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_tids ~loc x
    | _ -> raise (Failure "error in state 7")

  (* ITEMS:
       tids → tid tids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_8 ~loc a0_tids a1_tid _c0_tids =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a27_tids ~loc a0_tids a1_tid ()
      and loc = reduce_loc ~loc 2 in
      _c0_tids ~loc x
    | _ -> raise (Failure "error in state 8")

  (* ITEMS:
       decl → DTOKEN tp . tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . tid tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tid → . TID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       TID -> 4
       tids -> 10
       tid -> 7
     ACTION:
       TID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_9 ~loc a0_tp _c0_decl =
    let rec _c1_tids ~loc x = state_10 ~loc x a0_tp _c0_decl
    and _c2_tid ~loc x = state_7 ~loc x _c1_tids in
    match lookahead () with
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c2_tid
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a28_tids ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_tids ~loc x
    | _ -> raise (Failure "error in state 9")

  (* ITEMS:
       decl → DTOKEN tp tids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_10 ~loc a0_tids a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a3_decl ~loc a0_tids a1_tp ()
      and loc = reduce_loc ~loc 3 in
      _c0_decl ~loc x
    | _ -> raise (Failure "error in state 10")

  (* ITEMS:
       decl → DTYPE . tp symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tp → . TYPE /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       TYPE -> 5
       tp -> 12
     ACTION:
       TYPE -> shift *)
  and state_11 ~loc _c0_decl =
    let rec _c1_tp ~loc x = state_12 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | TYPE x ->
      let loc = shift () :: loc in
      state_5 ~loc x _c1_tp
    | _ -> raise (Failure "error in state 11")

  (* ITEMS:
       decl → DTYPE tp . symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . symbol symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . id /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . tid /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       id → . ID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tid → . TID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 13
       TID -> 4
       symbols -> 14
       symbol -> 15
       id -> 17
       tid -> 18
     ACTION:
       ID TID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_12 ~loc a0_tp _c0_decl =
    let rec _c1_symbols ~loc x = state_14 ~loc x a0_tp _c0_decl
    and _c2_symbol ~loc x = state_15 ~loc x _c1_symbols
    and _c3_id ~loc x = state_17 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_18 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c4_tid
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a30_symbols ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_symbols ~loc x
    | _ -> raise (Failure "error in state 12")

  (* ITEMS:
       id → ID . /ID /TID /CODE /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DPREC /DSEP /COLON /EQ
     GOTO:
       
     ACTION:
       ID TID CODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP COLON EQ -> reduce 0 0 *)
  and state_13 ~loc a0_ID _c0_id =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DPREC | DSEP | COLON | EQ ->
      let x = Actions.a33_id ~loc a0_ID ()
      and loc = reduce_loc ~loc 1 in
      _c0_id ~loc x
    | _ -> raise (Failure "error in state 13")

  (* ITEMS:
       decl → DTYPE tp symbols . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_14 ~loc a0_symbols a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a5_decl ~loc a0_symbols a1_tp ()
      and loc = reduce_loc ~loc 3 in
      _c0_decl ~loc x
    | _ -> raise (Failure "error in state 14")

  (* ITEMS:
       symbols → symbol . symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . symbol symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . id /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . tid /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       id → . ID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tid → . TID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 13
       TID -> 4
       symbols -> 16
       symbol -> 15
       id -> 17
       tid -> 18
     ACTION:
       ID TID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_15 ~loc a0_symbol _c0_symbols =
    let rec _c1_symbols ~loc x = state_16 ~loc x a0_symbol _c0_symbols
    and _c2_symbol ~loc x = state_15 ~loc x _c1_symbols
    and _c3_id ~loc x = state_17 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_18 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c4_tid
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a30_symbols ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_symbols ~loc x
    | _ -> raise (Failure "error in state 15")

  (* ITEMS:
       symbols → symbol symbols . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_16 ~loc a0_symbols a1_symbol _c0_symbols =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a29_symbols ~loc a0_symbols a1_symbol ()
      and loc = reduce_loc ~loc 2 in
      _c0_symbols ~loc x
    | _ -> raise (Failure "error in state 16")

  (* ITEMS:
       symbol → id . /ID /TID /CODE /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DPREC /DSEP
     GOTO:
       
     ACTION:
       ID TID CODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP -> reduce 0 0 *)
  and state_17 ~loc a0_id _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DPREC | DSEP ->
      let x = Actions.a31_symbol ~loc a0_id ()
      and loc = reduce_loc ~loc 1 in
      _c0_symbol ~loc x
    | _ -> raise (Failure "error in state 17")

  (* ITEMS:
       symbol → tid . /ID /TID /CODE /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DPREC /DSEP
     GOTO:
       
     ACTION:
       ID TID CODE DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DPREC DSEP -> reduce 0 0 *)
  and state_18 ~loc a0_tid _c0_symbol =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DPREC | DSEP ->
      let x = Actions.a32_symbol ~loc a0_tid ()
      and loc = reduce_loc ~loc 1 in
      _c0_symbol ~loc x
    | _ -> raise (Failure "error in state 18")

  (* ITEMS:
       decl → DSTART . tp ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → DSTART . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . id ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       id → . ID /ID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tp → . TYPE /ID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 13
       TYPE -> 5
       ids -> 20
       id -> 21
       tp -> 23
     ACTION:
       ID TYPE -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_19 ~loc _c0_decl =
    let rec _c1_ids ~loc x = state_20 ~loc x _c0_decl
    and _c2_id ~loc x = state_21 ~loc x _c1_ids
    and _c3_tp ~loc x = state_23 ~loc x _c0_decl in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c2_id
    (* Shift *)
    | TYPE x ->
      let loc = shift () :: loc in
      state_5 ~loc x _c3_tp
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a26_ids ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_ids ~loc x
    | _ -> raise (Failure "error in state 19")

  (* ITEMS:
       decl → DSTART ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_20 ~loc a0_ids _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a7_decl ~loc a0_ids ()
      and loc = reduce_loc ~loc 2 in
      _c0_decl ~loc x
    | _ -> raise (Failure "error in state 20")

  (* ITEMS:
       ids → id . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . id ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       id → . ID /ID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 13
       ids -> 22
       id -> 21
     ACTION:
       ID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_21 ~loc a0_id _c0_ids =
    let rec _c1_ids ~loc x = state_22 ~loc x a0_id _c0_ids
    and _c2_id ~loc x = state_21 ~loc x _c1_ids in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c2_id
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a26_ids ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_ids ~loc x
    | _ -> raise (Failure "error in state 21")

  (* ITEMS:
       ids → id ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_22 ~loc a0_ids a1_id _c0_ids =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a25_ids ~loc a0_ids a1_id ()
      and loc = reduce_loc ~loc 2 in
      _c0_ids ~loc x
    | _ -> raise (Failure "error in state 22")

  (* ITEMS:
       decl → DSTART tp . ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . id ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       ids → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       id → . ID /ID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 13
       ids -> 24
       id -> 21
     ACTION:
       ID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_23 ~loc a0_tp _c0_decl =
    let rec _c1_ids ~loc x = state_24 ~loc x a0_tp _c0_decl
    and _c2_id ~loc x = state_21 ~loc x _c1_ids in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c2_id
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a26_ids ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_ids ~loc x
    | _ -> raise (Failure "error in state 23")

  (* ITEMS:
       decl → DSTART tp ids . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_24 ~loc a0_ids a1_tp _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a4_decl ~loc a0_ids a1_tp ()
      and loc = reduce_loc ~loc 3 in
      _c0_decl ~loc x
    | _ -> raise (Failure "error in state 24")

  (* ITEMS:
       decl → DLEFT . symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . symbol symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . id /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . tid /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       id → . ID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tid → . TID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 13
       TID -> 4
       symbols -> 26
       symbol -> 15
       id -> 17
       tid -> 18
     ACTION:
       ID TID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_25 ~loc _c0_decl =
    let rec _c1_symbols ~loc x = state_26 ~loc x _c0_decl
    and _c2_symbol ~loc x = state_15 ~loc x _c1_symbols
    and _c3_id ~loc x = state_17 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_18 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c4_tid
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a30_symbols ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_symbols ~loc x
    | _ -> raise (Failure "error in state 25")

  (* ITEMS:
       decl → DLEFT symbols . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_26 ~loc a0_symbols _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a8_decl ~loc a0_symbols ()
      and loc = reduce_loc ~loc 2 in
      _c0_decl ~loc x
    | _ -> raise (Failure "error in state 26")

  (* ITEMS:
       decl → DRIGHT . symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . symbol symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . id /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . tid /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       id → . ID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tid → . TID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 13
       TID -> 4
       symbols -> 28
       symbol -> 15
       id -> 17
       tid -> 18
     ACTION:
       ID TID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_27 ~loc _c0_decl =
    let rec _c1_symbols ~loc x = state_28 ~loc x _c0_decl
    and _c2_symbol ~loc x = state_15 ~loc x _c1_symbols
    and _c3_id ~loc x = state_17 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_18 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c4_tid
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a30_symbols ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_symbols ~loc x
    | _ -> raise (Failure "error in state 27")

  (* ITEMS:
       decl → DRIGHT symbols . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_28 ~loc a0_symbols _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a9_decl ~loc a0_symbols ()
      and loc = reduce_loc ~loc 2 in
      _c0_decl ~loc x
    | _ -> raise (Failure "error in state 28")

  (* ITEMS:
       decl → DNONASSOC . symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . symbol symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbols → . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . id /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       symbol → . tid /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       id → . ID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       tid → . TID /ID /TID /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       ID -> 13
       TID -> 4
       symbols -> 30
       symbol -> 15
       id -> 17
       tid -> 18
     ACTION:
       ID TID -> shift
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 1 1 *)
  and state_29 ~loc _c0_decl =
    let rec _c1_symbols ~loc x = state_30 ~loc x _c0_decl
    and _c2_symbol ~loc x = state_15 ~loc x _c1_symbols
    and _c3_id ~loc x = state_17 ~loc x _c2_symbol
    and _c4_tid ~loc x = state_18 ~loc x _c2_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c3_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c4_tid
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a30_symbols ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_symbols ~loc x
    | _ -> raise (Failure "error in state 29")

  (* ITEMS:
       decl → DNONASSOC symbols . /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC DSEP -> reduce 0 0 *)
  and state_30 ~loc a0_symbols _c0_decl =
    match lookahead () with
    (* Reduce *)
    | DTOKEN | DTYPE | DSTART | DLEFT | DRIGHT | DNONASSOC | DSEP ->
      let x = Actions.a10_decl ~loc a0_symbols ()
      and loc = reduce_loc ~loc 2 in
      _c0_decl ~loc x
    | _ -> raise (Failure "error in state 30")

  (* ITEMS:
       grammar' → code decls . DSEP rules EOF
     GOTO:
       DSEP -> 32
     ACTION:
       DSEP -> shift *)
  and state_31 ~loc a0_decls a1_code _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | DSEP ->
      let loc = shift () :: loc in
      state_32 ~loc a0_decls a1_code _c0_grammar_starting
    | _ -> raise (Failure "error in state 31")

  (* ITEMS:
       grammar' → code decls DSEP . rules EOF
       rules → . rule rules /EOF
       rules → . /EOF
       rule → . id COLON rule_prods SEMI /ID /EOF
       id → . ID /COLON
     GOTO:
       ID -> 13
       rules -> 33
       rule -> 35
       id -> 37
     ACTION:
       ID -> shift
       EOF -> reduce 1 1 *)
  and state_32 ~loc a1_decls a2_code _c0_grammar_starting =
    let rec _c1_rules ~loc x = state_33 ~loc x a1_decls a2_code _c0_grammar_starting
    and _c2_rule ~loc x = state_35 ~loc x _c1_rules
    and _c3_id ~loc x = state_37 ~loc x _c2_rule in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c3_id
    (* Reduce *)
    | EOF ->
      let x = Actions.a12_rules ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_rules ~loc x
    | _ -> raise (Failure "error in state 32")

  (* ITEMS:
       grammar' → code decls DSEP rules . EOF
     GOTO:
       EOF -> 34
     ACTION:
       EOF -> shift *)
  and state_33 ~loc a0_rules a2_decls a3_code _c0_grammar_starting =
    match lookahead () with
    (* Shift *)
    | EOF ->
      let loc = shift () :: loc in
      state_34 ~loc a0_rules a2_decls a3_code _c0_grammar_starting
    | _ -> raise (Failure "error in state 33")

  (* ITEMS:
       grammar' → code decls DSEP rules EOF .
     GOTO:
       
     ACTION:
        *)
  and state_34 ~loc a1_rules a3_decls a4_code _c0_grammar_starting =
    (* Reduce *)
    let x = Actions.a0_grammar ~loc a1_rules a3_decls a4_code () in
    _c0_grammar_starting x

  (* ITEMS:
       rules → rule . rules /EOF
       rules → . rule rules /EOF
       rules → . /EOF
       rule → . id COLON rule_prods SEMI /ID /EOF
       id → . ID /COLON
     GOTO:
       ID -> 13
       rules -> 36
       rule -> 35
       id -> 37
     ACTION:
       ID -> shift
       EOF -> reduce 1 1 *)
  and state_35 ~loc a0_rule _c0_rules =
    let rec _c1_rules ~loc x = state_36 ~loc x a0_rule _c0_rules
    and _c2_rule ~loc x = state_35 ~loc x _c1_rules
    and _c3_id ~loc x = state_37 ~loc x _c2_rule in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c3_id
    (* Reduce *)
    | EOF ->
      let x = Actions.a12_rules ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_rules ~loc x
    | _ -> raise (Failure "error in state 35")

  (* ITEMS:
       rules → rule rules . /EOF
     GOTO:
       
     ACTION:
       EOF -> reduce 0 0 *)
  and state_36 ~loc a0_rules a1_rule _c0_rules =
    match lookahead () with
    (* Reduce *)
    | EOF ->
      let x = Actions.a11_rules ~loc a0_rules a1_rule ()
      and loc = reduce_loc ~loc 2 in
      _c0_rules ~loc x
    | _ -> raise (Failure "error in state 36")

  (* ITEMS:
       rule → id . COLON rule_prods SEMI /ID /EOF
     GOTO:
       COLON -> 38
     ACTION:
       COLON -> shift *)
  and state_37 ~loc a0_id _c0_rule =
    match lookahead () with
    (* Shift *)
    | COLON ->
      let loc = shift () :: loc in
      state_38 ~loc a0_id _c0_rule
    | _ -> raise (Failure "error in state 37")

  (* ITEMS:
       rule → id COLON . rule_prods SEMI /ID /EOF
       rule_prods → . production productions /SEMI
       rule_prods → . productions /SEMI
       productions → . BAR production productions /SEMI
       productions → . /SEMI
       production → . producers production_prec code /SEMI /BAR
       producers → . producer producers /CODE /DPREC
       producers → . /CODE /DPREC
       producer → . id EQ symbol /ID /TID /CODE /DPREC
       producer → . symbol /ID /TID /CODE /DPREC
       symbol → . id /ID /TID /CODE /DPREC
       symbol → . tid /ID /TID /CODE /DPREC
       id → . ID /ID /TID /CODE /DPREC /EQ
       tid → . TID /ID /TID /CODE /DPREC
     GOTO:
       ID -> 13
       TID -> 4
       BAR -> 39
       rule_prods -> 53
       productions -> 55
       production -> 56
       producers -> 42
       producer -> 47
       symbol -> 49
       id -> 50
       tid -> 18
     ACTION:
       CODE DPREC -> reduce 4 1
       SEMI -> reduce 2 1
       ID TID BAR -> shift *)
  and state_38 ~loc a1_id _c0_rule =
    let rec _c1_rule_prods ~loc x = state_53 ~loc x a1_id _c0_rule
    and _c2_productions ~loc x = state_55 ~loc x _c1_rule_prods
    and _c3_production ~loc x = state_56 ~loc x _c1_rule_prods
    and _c4_producers ~loc x = state_42 ~loc x _c3_production
    and _c5_producer ~loc x = state_47 ~loc x _c4_producers
    and _c6_symbol ~loc x = state_49 ~loc x _c5_producer
    and _c7_id ~loc x = state_50 ~loc x _c5_producer _c6_symbol
    and _c8_tid ~loc x = state_18 ~loc x _c6_symbol in
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let x = Actions.a22_producers ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c4_producers ~loc x
    (* Reduce *)
    | SEMI ->
      let x = Actions.a17_productions ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c2_productions ~loc x
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c7_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c8_tid
    (* Shift *)
    | BAR ->
      let loc = shift () :: loc in
      state_39 ~loc _c2_productions
    | _ -> raise (Failure "error in state 38")

  (* ITEMS:
       productions → BAR . production productions /SEMI
       production → . producers production_prec code /SEMI /BAR
       producers → . producer producers /CODE /DPREC
       producers → . /CODE /DPREC
       producer → . id EQ symbol /ID /TID /CODE /DPREC
       producer → . symbol /ID /TID /CODE /DPREC
       symbol → . id /ID /TID /CODE /DPREC
       symbol → . tid /ID /TID /CODE /DPREC
       id → . ID /ID /TID /CODE /DPREC /EQ
       tid → . TID /ID /TID /CODE /DPREC
     GOTO:
       ID -> 13
       TID -> 4
       production -> 40
       producers -> 42
       producer -> 47
       symbol -> 49
       id -> 50
       tid -> 18
     ACTION:
       ID TID -> shift
       CODE DPREC -> reduce 2 1 *)
  and state_39 ~loc _c0_productions =
    let rec _c1_production ~loc x = state_40 ~loc x _c0_productions
    and _c2_producers ~loc x = state_42 ~loc x _c1_production
    and _c3_producer ~loc x = state_47 ~loc x _c2_producers
    and _c4_symbol ~loc x = state_49 ~loc x _c3_producer
    and _c5_id ~loc x = state_50 ~loc x _c3_producer _c4_symbol
    and _c6_tid ~loc x = state_18 ~loc x _c4_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c5_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c6_tid
    (* Reduce *)
    | CODE _ | DPREC ->
      let x = Actions.a22_producers ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c2_producers ~loc x
    | _ -> raise (Failure "error in state 39")

  (* ITEMS:
       productions → BAR production . productions /SEMI
       productions → . BAR production productions /SEMI
       productions → . /SEMI
     GOTO:
       BAR -> 39
       productions -> 41
     ACTION:
       BAR -> shift
       SEMI -> reduce 1 1 *)
  and state_40 ~loc a0_production _c0_productions =
    let rec _c1_productions ~loc x = state_41 ~loc x a0_production _c0_productions in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let loc = shift () :: loc in
      state_39 ~loc _c1_productions
    (* Reduce *)
    | SEMI ->
      let x = Actions.a17_productions ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_productions ~loc x
    | _ -> raise (Failure "error in state 40")

  (* ITEMS:
       productions → BAR production productions . /SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_41 ~loc a0_productions a1_production _c0_productions =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let x = Actions.a16_productions ~loc a0_productions a1_production ()
      and loc = reduce_loc ~loc 3 in
      _c0_productions ~loc x
    | _ -> raise (Failure "error in state 41")

  (* ITEMS:
       production → producers . production_prec code /SEMI /BAR
       production_prec → . DPREC symbol /CODE
       production_prec → . /CODE
     GOTO:
       DPREC -> 43
       production_prec -> 45
     ACTION:
       DPREC -> shift
       CODE -> reduce 1 1 *)
  and state_42 ~loc a0_producers _c0_production =
    let rec _c1_production_prec ~loc x = state_45 ~loc x a0_producers _c0_production in
    match lookahead () with
    (* Shift *)
    | DPREC ->
      let loc = shift () :: loc in
      state_43 ~loc _c1_production_prec
    (* Reduce *)
    | CODE _ ->
      let x = Actions.a20_production_prec ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_production_prec ~loc x
    | _ -> raise (Failure "error in state 42")

  (* ITEMS:
       production_prec → DPREC . symbol /CODE
       symbol → . id /CODE
       symbol → . tid /CODE
       id → . ID /CODE
       tid → . TID /CODE
     GOTO:
       ID -> 13
       TID -> 4
       symbol -> 44
       id -> 17
       tid -> 18
     ACTION:
       ID TID -> shift *)
  and state_43 ~loc _c0_production_prec =
    let rec _c1_symbol ~loc x = state_44 ~loc x _c0_production_prec
    and _c2_id ~loc x = state_17 ~loc x _c1_symbol
    and _c3_tid ~loc x = state_18 ~loc x _c1_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c3_tid
    | _ -> raise (Failure "error in state 43")

  (* ITEMS:
       production_prec → DPREC symbol . /CODE
     GOTO:
       
     ACTION:
       CODE -> reduce 0 0 *)
  and state_44 ~loc a0_symbol _c0_production_prec =
    match lookahead () with
    (* Reduce *)
    | CODE _ ->
      let x = Actions.a19_production_prec ~loc a0_symbol ()
      and loc = reduce_loc ~loc 2 in
      _c0_production_prec ~loc x
    | _ -> raise (Failure "error in state 44")

  (* ITEMS:
       production → producers production_prec . code /SEMI /BAR
       code → . CODE /SEMI /BAR
     GOTO:
       CODE -> 1
       code -> 46
     ACTION:
       CODE -> shift *)
  and state_45 ~loc a0_production_prec a1_producers _c0_production =
    let rec _c1_code ~loc x = state_46 ~loc x a0_production_prec a1_producers _c0_production in
    match lookahead () with
    (* Shift *)
    | CODE x ->
      let loc = shift () :: loc in
      state_1 ~loc x _c1_code
    | _ -> raise (Failure "error in state 45")

  (* ITEMS:
       production → producers production_prec code . /SEMI /BAR
     GOTO:
       
     ACTION:
       SEMI BAR -> reduce 0 0 *)
  and state_46 ~loc a0_code a1_production_prec a2_producers _c0_production =
    match lookahead () with
    (* Reduce *)
    | SEMI | BAR ->
      let x = Actions.a18_production ~loc a0_code a1_production_prec a2_producers ()
      and loc = reduce_loc ~loc 3 in
      _c0_production ~loc x
    | _ -> raise (Failure "error in state 46")

  (* ITEMS:
       producers → producer . producers /CODE /DPREC
       producers → . producer producers /CODE /DPREC
       producers → . /CODE /DPREC
       producer → . id EQ symbol /ID /TID /CODE /DPREC
       producer → . symbol /ID /TID /CODE /DPREC
       symbol → . id /ID /TID /CODE /DPREC
       symbol → . tid /ID /TID /CODE /DPREC
       id → . ID /ID /TID /CODE /DPREC /EQ
       tid → . TID /ID /TID /CODE /DPREC
     GOTO:
       ID -> 13
       TID -> 4
       producers -> 48
       producer -> 47
       symbol -> 49
       id -> 50
       tid -> 18
     ACTION:
       ID TID -> shift
       CODE DPREC -> reduce 1 1 *)
  and state_47 ~loc a0_producer _c0_producers =
    let rec _c1_producers ~loc x = state_48 ~loc x a0_producer _c0_producers
    and _c2_producer ~loc x = state_47 ~loc x _c1_producers
    and _c3_symbol ~loc x = state_49 ~loc x _c2_producer
    and _c4_id ~loc x = state_50 ~loc x _c2_producer _c3_symbol
    and _c5_tid ~loc x = state_18 ~loc x _c3_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c4_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c5_tid
    (* Reduce *)
    | CODE _ | DPREC ->
      let x = Actions.a22_producers ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_producers ~loc x
    | _ -> raise (Failure "error in state 47")

  (* ITEMS:
       producers → producer producers . /CODE /DPREC
     GOTO:
       
     ACTION:
       CODE DPREC -> reduce 0 0 *)
  and state_48 ~loc a0_producers a1_producer _c0_producers =
    match lookahead () with
    (* Reduce *)
    | CODE _ | DPREC ->
      let x = Actions.a21_producers ~loc a0_producers a1_producer ()
      and loc = reduce_loc ~loc 2 in
      _c0_producers ~loc x
    | _ -> raise (Failure "error in state 48")

  (* ITEMS:
       producer → symbol . /ID /TID /CODE /DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DPREC -> reduce 0 0 *)
  and state_49 ~loc a0_symbol _c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let x = Actions.a24_producer ~loc a0_symbol ()
      and loc = reduce_loc ~loc 1 in
      _c0_producer ~loc x
    | _ -> raise (Failure "error in state 49")

  (* ITEMS:
       producer → id . EQ symbol /ID /TID /CODE /DPREC
       symbol → id . /ID /TID /CODE /DPREC
     GOTO:
       EQ -> 51
     ACTION:
       EQ -> shift
       ID TID CODE DPREC -> reduce 1 0 *)
  and state_50 ~loc a0_id _c0_producer _c1_symbol =
    match lookahead () with
    (* Shift *)
    | EQ ->
      let loc = shift () :: loc in
      state_51 ~loc a0_id _c0_producer
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let x = Actions.a31_symbol ~loc a0_id ()
      and loc = reduce_loc ~loc 1 in
      _c1_symbol ~loc x
    | _ -> raise (Failure "error in state 50")

  (* ITEMS:
       producer → id EQ . symbol /ID /TID /CODE /DPREC
       symbol → . id /ID /TID /CODE /DPREC
       symbol → . tid /ID /TID /CODE /DPREC
       id → . ID /ID /TID /CODE /DPREC
       tid → . TID /ID /TID /CODE /DPREC
     GOTO:
       ID -> 13
       TID -> 4
       symbol -> 52
       id -> 17
       tid -> 18
     ACTION:
       ID TID -> shift *)
  and state_51 ~loc a1_id _c0_producer =
    let rec _c1_symbol ~loc x = state_52 ~loc x a1_id _c0_producer
    and _c2_id ~loc x = state_17 ~loc x _c1_symbol
    and _c3_tid ~loc x = state_18 ~loc x _c1_symbol in
    match lookahead () with
    (* Shift *)
    | ID x ->
      let loc = shift () :: loc in
      state_13 ~loc x _c2_id
    (* Shift *)
    | TID x ->
      let loc = shift () :: loc in
      state_4 ~loc x _c3_tid
    | _ -> raise (Failure "error in state 51")

  (* ITEMS:
       producer → id EQ symbol . /ID /TID /CODE /DPREC
     GOTO:
       
     ACTION:
       ID TID CODE DPREC -> reduce 0 0 *)
  and state_52 ~loc a0_symbol a2_id _c0_producer =
    match lookahead () with
    (* Reduce *)
    | ID _ | TID _ | CODE _ | DPREC ->
      let x = Actions.a23_producer ~loc a0_symbol a2_id ()
      and loc = reduce_loc ~loc 3 in
      _c0_producer ~loc x
    | _ -> raise (Failure "error in state 52")

  (* ITEMS:
       rule → id COLON rule_prods . SEMI /ID /EOF
     GOTO:
       SEMI -> 54
     ACTION:
       SEMI -> shift *)
  and state_53 ~loc a0_rule_prods a2_id _c0_rule =
    match lookahead () with
    (* Shift *)
    | SEMI ->
      let loc = shift () :: loc in
      state_54 ~loc a0_rule_prods a2_id _c0_rule
    | _ -> raise (Failure "error in state 53")

  (* ITEMS:
       rule → id COLON rule_prods SEMI . /ID /EOF
     GOTO:
       
     ACTION:
       ID EOF -> reduce 0 0 *)
  and state_54 ~loc a1_rule_prods a3_id _c0_rule =
    match lookahead () with
    (* Reduce *)
    | ID _ | EOF ->
      let x = Actions.a13_rule ~loc a1_rule_prods a3_id ()
      and loc = reduce_loc ~loc 4 in
      _c0_rule ~loc x
    | _ -> raise (Failure "error in state 54")

  (* ITEMS:
       rule_prods → productions . /SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_55 ~loc a0_productions _c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let x = Actions.a15_rule_prods ~loc a0_productions ()
      and loc = reduce_loc ~loc 1 in
      _c0_rule_prods ~loc x
    | _ -> raise (Failure "error in state 55")

  (* ITEMS:
       rule_prods → production . productions /SEMI
       productions → . BAR production productions /SEMI
       productions → . /SEMI
     GOTO:
       BAR -> 39
       productions -> 57
     ACTION:
       BAR -> shift
       SEMI -> reduce 1 1 *)
  and state_56 ~loc a0_production _c0_rule_prods =
    let rec _c1_productions ~loc x = state_57 ~loc x a0_production _c0_rule_prods in
    match lookahead () with
    (* Shift *)
    | BAR ->
      let loc = shift () :: loc in
      state_39 ~loc _c1_productions
    (* Reduce *)
    | SEMI ->
      let x = Actions.a17_productions ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_productions ~loc x
    | _ -> raise (Failure "error in state 56")

  (* ITEMS:
       rule_prods → production productions . /SEMI
     GOTO:
       
     ACTION:
       SEMI -> reduce 0 0 *)
  and state_57 ~loc a0_productions a1_production _c0_rule_prods =
    match lookahead () with
    (* Reduce *)
    | SEMI ->
      let x = Actions.a14_rule_prods ~loc a0_productions a1_production ()
      and loc = reduce_loc ~loc 2 in
      _c0_rule_prods ~loc x
    | _ -> raise (Failure "error in state 57")

  (* ITEMS:
       decls → decl . decls /DSEP
       decls → . decl decls /DSEP
       decls → . /DSEP
       decl → . DTOKEN tp tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DSTART tp ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DTYPE tp symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DTOKEN tids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DSTART ids /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DLEFT symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DRIGHT symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
       decl → . DNONASSOC symbols /DTOKEN /DTYPE /DSTART /DLEFT /DRIGHT /DNONASSOC /DSEP
     GOTO:
       DTOKEN -> 3
       DTYPE -> 11
       DSTART -> 19
       DLEFT -> 25
       DRIGHT -> 27
       DNONASSOC -> 29
       decls -> 59
       decl -> 58
     ACTION:
       DTOKEN DTYPE DSTART DLEFT DRIGHT DNONASSOC -> shift
       DSEP -> reduce 1 1 *)
  and state_58 ~loc a0_decl _c0_decls =
    let rec _c1_decls ~loc x = state_59 ~loc x a0_decl _c0_decls
    and _c2_decl ~loc x = state_58 ~loc x _c1_decls in
    match lookahead () with
    (* Shift *)
    | DTOKEN ->
      let loc = shift () :: loc in
      state_3 ~loc _c2_decl
    (* Shift *)
    | DTYPE ->
      let loc = shift () :: loc in
      state_11 ~loc _c2_decl
    (* Shift *)
    | DSTART ->
      let loc = shift () :: loc in
      state_19 ~loc _c2_decl
    (* Shift *)
    | DLEFT ->
      let loc = shift () :: loc in
      state_25 ~loc _c2_decl
    (* Shift *)
    | DRIGHT ->
      let loc = shift () :: loc in
      state_27 ~loc _c2_decl
    (* Shift *)
    | DNONASSOC ->
      let loc = shift () :: loc in
      state_29 ~loc _c2_decl
    (* Reduce *)
    | DSEP ->
      let x = Actions.a2_decls ~loc ()
      and loc = reduce_loc ~loc 0 in
      _c1_decls ~loc x
    | _ -> raise (Failure "error in state 58")

  (* ITEMS:
       decls → decl decls . /DSEP
     GOTO:
       
     ACTION:
       DSEP -> reduce 0 0 *)
  and state_59 ~loc a0_decls a1_decl _c0_decls =
    match lookahead () with
    (* Reduce *)
    | DSEP ->
      let x = Actions.a1_decls ~loc a0_decls a1_decl ()
      and loc = reduce_loc ~loc 2 in
      _c0_decls ~loc x
    | _ -> raise (Failure "error in state 59")
  ;;
end

let grammar lexfun lexbuf =
  States.setup lexfun lexbuf;
  States.state_0 ~loc:[] (fun x -> x)
;;
