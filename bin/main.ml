let usage = "usage: cpspg [options] sourcefile"
let source_name = ref None
let output_name = ref None
let output_automaton = ref None
let grammar_kind = ref Cpspg.Types.LALR
let codegen_line_directives = ref false (* Disabled for now *)
let codegen_comments = ref false
let codegen_readable_ids = ref false
let codegen_locations = ref true

let codegen_readable () =
  codegen_line_directives := false;
  codegen_readable_ids := true;
  codegen_comments := true
;;

let specs =
  [ ( "-o"
    , Arg.String (fun x -> output_name := Some x)
    , "<file> Set output file name to <file>" )
  ; ( "--automaton"
    , Arg.String (fun x -> output_automaton := Some x)
    , "<file> Dump automaton graph in .dot format to <file>" )
    (* Grammar kind *)
  ; ( "--lr0"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LR0)
    , "Construct a LR(0) automaton" )
  ; ( "--slr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.SLR)
    , "Construct a SLR(1) automaton" )
  ; ( "--lr1"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LR1)
    , "Construct a LR(1) automaton" )
  ; ( "--lalr"
    , Arg.Unit (fun _ -> grammar_kind := Cpspg.Types.LALR)
    , "Construct a LALR(1) automaton (default)" )
    (* Codegen options *)
  ; ( "--no-locations"
    , Arg.Unit (fun _ -> codegen_locations := false)
    , "Disable family of $loc keywords and related code" )
  ; ( "--no-line-directives"
    , Arg.Unit (fun _ -> codegen_line_directives := false)
    , "Do not include line directives in generated code" )
  ; "--comment", Arg.Set codegen_comments, "Include comments in the generated code"
  ; ( "--readable-ids"
    , Arg.Set codegen_readable_ids
    , "Make identifiers in generated code longer" )
  ; ( "--readable"
    , Arg.Unit codegen_readable
    , "Make generated code more readable (implies --comment, --readable-ids and \
       --no-line-directives)" )
  ]
  |> Arg.align
;;

let _ = Arg.parse specs (fun x -> source_name := Some x) usage

let main () =
  let input, input_name =
    match !source_name with
    | None | Some "-" -> stdin, "-"
    | Some x -> open_in x, x
  and output =
    match !output_name with
    | None | Some "-" -> stdout
    | Some x -> open_out x
  in
  let conflicts = ref [] in
  (* Settings *)
  let module Settings = struct
    let kind = !grammar_kind

    (* Codegen *)
    let locations = !codegen_locations
    let line_directives = !codegen_line_directives
    let comments = !codegen_comments
    let readable_ids = !codegen_readable_ids

    (* Reporting *)
    let report_err ~loc = Format.kdprintf (Warning.report_err ~loc:(fst loc))
    let report_warn ~loc = Format.kdprintf (Warning.report_warn ~loc:(fst loc))
    let report_conflict id sym actions = conflicts := (id, sym, actions) :: !conflicts
  end
  in
  (* First pass: parse grammar definition *)
  let module Ast = struct
    let lexbuf = Lexing.from_channel input
    let _ = Lexing.set_filename lexbuf input_name
    let ast = Cpspg.Parser.grammar Cpspg.Lexer.main lexbuf
  end
  in
  (* Second pass: create context-free grammar *)
  let module Grammar = Cpspg.GrammarGen.Run (Settings) (Ast) in
  (* Third pass: create LR automaton *)
  let module Automaton = Cpspg.AutomatonGen.Run (Settings) (Grammar) in
  let module Conflicts = Warning.Conflict (Grammar) (Automaton) in
  List.iter (fun (i, s, a) -> Conflicts.report i s a) !conflicts;
  (* Fourth pass: initialize code generation *)
  let module Code = Cpspg.CodeGen.Make (Settings) (Grammar) (Automaton) in
  let module Graphviz = Cpspg.Graphviz.Make (Grammar) in
  (* Final work *)
  Code.write (Format.formatter_of_out_channel output);
  match !output_automaton with
  | None -> ()
  | Some x ->
    let out = if x = "-" then stdout else open_out x in
    Graphviz.fmt_automaton (Format.formatter_of_out_channel out) Automaton.automaton
;;

let _ =
  main ();
  exit 0
;;
