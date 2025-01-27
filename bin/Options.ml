module Parse () = struct
  let usage =
    "usage:\n\
    \  cpspg [options] <input> [output...]\n\
    \  cpspg [options] [-f format] -o <output> <input>\n"
  ;;

  let input_name = ref None
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
    match !input_name with
    | Some _ -> output_names := x :: !output_names
    | None -> input_name := Some x
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

  (* Forward parsed options *)
  let input =
    match !input_name with
    | None | Some "-" -> stdin, "<stdin>"
    | Some x -> open_in x, x
  ;;

  let outputs =
    let f = function
      | "-" -> stdout, "<stdout>", Option.value ~default:".ml" !output_format
      | name ->
        let ext = Filename.extension name in
        open_out name, name, Option.value ~default:ext !output_format
    in
    if !output_names = []
    then [ stdout, "<stdout>", Option.value ~default:".ml" !output_format ]
    else List.map f !output_names
  ;;

  let kind = !grammar_kind
  let locations = !codegen_locations
  let compat = !codegen_compat
  let line_directives = !codegen_line_directives
  let comments = !codegen_comments
  let readable_ids = !codegen_readable_ids
end
