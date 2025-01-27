module Opts = Options.Parse ()

module Raw = struct
  let input, input_name = Opts.input
  let lexbuf = Lexing.from_channel input
  let _ = Lexing.set_filename lexbuf input_name

  let raw =
    try Cpspg.Parser.grammar Cpspg.Lexer.main lexbuf with
    | Parsing.Parse_error ->
      let loc = lexbuf.lex_start_p, lexbuf.lex_curr_p
      and lex = Lexing.lexeme lexbuf
      and exp = Cpspg.Parser.expected_tokens () in
      let exp = String.concat ", " exp in
      Logger.report_err ~loc "Unexpected token `%s', expected %s" lex exp;
      Cpspg.Raw.dummy
    | Cpspg.Lexer.UnexpectedInput (Some c) ->
      let loc = lexbuf.lex_start_p, lexbuf.lex_curr_p in
      Logger.report_err ~loc "Unexpected character `%c'" c;
      Cpspg.Raw.dummy
    | Cpspg.Lexer.UnexpectedInput None ->
      let loc = lexbuf.lex_start_p, lexbuf.lex_curr_p in
      Logger.report_err ~loc "Unexpected end of input";
      Cpspg.Raw.dummy
  ;;
end

module Settings = struct
  include Logger
  include Opts
end

module Grammar = Cpspg.GrammarGen.Run (Settings) (Raw)
module Automaton = Cpspg.AutomatonGen.Run (Settings) (Grammar)
module Conflicts = Logger.Conflict (Grammar) (Automaton)

let write (out, name, format) =
  let module Settings = struct
    include Logger
    include Opts

    let out = out
  end
  in
  let module Code =
    (val match format with
         | ".ml" -> (module Cpspg.CodeGenMl.Make (Settings) (Grammar) (Automaton))
         | ".mli" -> (module Cpspg.CodeGenMli.Make (Settings) (Grammar) (Automaton))
         | ".dot" -> (module Cpspg.CodeGenDot.Make (Settings) (Grammar) (Automaton))
         | _ ->
           if format == ""
           then Logger.report_err "could not determine output format for %s" name
           else Logger.report_err "unsupported output format %s" format;
           exit 1
      : Cpspg.Types.Code)
  in
  Code.write ()
;;

let _ = if Logger.has_error () then exit 1 else List.iter write Opts.outputs
