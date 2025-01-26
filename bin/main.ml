let usage =
  "usage:\n\
  \  cpspg [options] <input> <output> ...\n\
  \  cpspg [options] -o <output> <input>\n"
;;

let source_name = ref None
let output_names = ref []
let output_format = ref None
let grammar_kind = ref Cpspg.Types.LALR
let codegen_line_directives = ref true
let codegen_comments = ref false
let codegen_readable_ids = ref false
let codegen_locations = ref true
let codegen_compat = ref false

let codegen_readable () =
  codegen_line_directives := false;
  codegen_readable_ids := true;
  codegen_comments := true
;;

let positional_arg x =
  match !source_name with
  | Some _ -> output_names := x :: !output_names
  | None -> source_name := Some x
;;

let specs =
  [ ( "-o"
    , Arg.String (fun x -> output_names := x :: !output_names)
    , "<file>\tSet output file name to <file>" )
  ; ( "-f"
    , Arg.String (fun x -> output_format := Some ("." ^ x))
    , "<format>\tSet output format to <format> (default: detect)" )
    (* Grammar kind *)
  ; ( "--lr0"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LR0)
    , "\tConstruct a LR(0) automaton" )
  ; ( "--slr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.SLR)
    , "\tConstruct a SLR(1) automaton" )
  ; ( "--lr1"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LR1)
    , "\tConstruct a LR(1) automaton" )
  ; ( "--lalr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LALR)
    , "\tConstruct a LALR(1) automaton (default)" )
    (* Codegen options *)
  ; ( "--no-locations"
    , Arg.Unit (fun _ -> codegen_locations := false)
    , "\tDisable family of $loc keywords and related code" )
  ; ( "--no-line-directives"
    , Arg.Unit (fun _ -> codegen_line_directives := false)
    , "\tDo not include line directives in generated code" )
  ; "--comment", Arg.Set codegen_comments, "\tInclude comments in the generated code"
  ; ( "--readable-ids"
    , Arg.Set codegen_readable_ids
    , "\tMake identifiers in generated code longer" )
  ; ( "--readable"
    , Arg.Unit codegen_readable
    , "\tMake generated code more readable (implies --comment, --readable-ids and \
       --no-line-directives)" )
  ; ( "--compat"
    , Arg.Set codegen_compat
    , "\tGenerate code with OCaml's Parsing module compability" )
  ]
  |> Arg.align
;;

let _ = Arg.parse specs positional_arg usage

let main () =
  let input, input_name =
    match !source_name with
    | None | Some "-" -> stdin, "-"
    | Some x -> open_in x, x
  in
  (* Settings *)
  let module Settings = struct
    let kind = !grammar_kind

    (* Codegen *)
    let locations = !codegen_locations
    let compat = !codegen_compat
    let line_directives = !codegen_line_directives
    let comments = !codegen_comments
    let readable_ids = !codegen_readable_ids

    (* Reporting *)
    let conflicts = ref []
    let has_error = ref false

    let report_err ?loc =
      has_error := true;
      let loc = Option.map fst loc in
      Format.kdprintf (Warning.report_err ?loc)
    ;;

    let report_warn ?loc =
      let loc = Option.map fst loc in
      Format.kdprintf (Warning.report_warn ?loc)
    ;;

    let report_conflict id sym actions =
      has_error := true;
      conflicts := (id, sym, actions) :: !conflicts
    ;;
  end
  in
  (* First pass: parse grammar definition *)
  let module Raw = struct
    let lexbuf = Lexing.from_channel input
    let _ = Lexing.set_filename lexbuf input_name

    let raw =
      try Cpspg.Parser.grammar Cpspg.Lexer.main lexbuf with
      | Parsing.Parse_error ->
        let loc = lexbuf.lex_start_p, lexbuf.lex_curr_p
        and lex = Lexing.lexeme lexbuf
        and exp = Cpspg.Parser.expected_tokens () in
        let exp = String.concat ", " exp in
        Settings.report_err ~loc "Unexpected token `%s', expected %s" lex exp;
        Cpspg.Raw.dummy
      | Cpspg.Lexer.UnexpectedInput (Some c) ->
        let loc = lexbuf.lex_start_p, lexbuf.lex_curr_p in
        Settings.report_err ~loc "Unexpected character `%c'" c;
        Cpspg.Raw.dummy
      | Cpspg.Lexer.UnexpectedInput None ->
        let loc = lexbuf.lex_start_p, lexbuf.lex_curr_p in
        Settings.report_err ~loc "Unexpected end of input";
        Cpspg.Raw.dummy
    ;;
  end
  in
  (* Second pass: create context-free grammar *)
  let module Grammar = Cpspg.GrammarGen.Run (Settings) (Raw) in
  (* Third pass: create LR automaton *)
  let module Automaton = Cpspg.AutomatonGen.Run (Settings) (Grammar) in
  let module Conflicts = Warning.Conflict (Grammar) (Automaton) in
  List.iter (fun (i, s, a) -> Conflicts.report i s a) !Settings.conflicts;
  (* Fourth pass: initialize code generation *)
  let write format output =
    let module Code =
      (val match format with
           | ".ml" -> (module Cpspg.CodeGenMl.Make (Settings) (Grammar) (Automaton))
           | ".mli" -> (module Cpspg.CodeGenMli.Make (Settings) (Grammar) (Automaton))
           | ".dot" -> (module Cpspg.CodeGenDot.Make (Settings) (Grammar) (Automaton))
           | _ -> failwith "Unknown output format"
        : Cpspg.Types.Code)
    in
    if not !Settings.has_error then Code.write (Format.formatter_of_out_channel output)
  in
  let get_format file = function
    | Some f -> f
    | None -> Filename.extension file
  in
  (* Write results *)
  (match !output_names with
   | [] -> write ".ml" stdout
   | xs -> List.iter (fun x -> write (get_format x !output_format) (open_out x)) xs);
  Bool.to_int !Settings.has_error
;;

let _ = main () |> exit
