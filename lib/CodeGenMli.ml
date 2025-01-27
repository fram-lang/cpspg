let parsing_compat_mod = "module Parsing : module type of Stdlib.Parsing\n"

let epilogue =
  "val error_token : unit -> token option\nval expected_tokens : unit -> string list\n"
;;

(* TODO: Maybe share some code with CodeGenMl.ml? *)
module Make (S : Types.BackSettings) (G : Types.Grammar) (A : Types.Automaton) :
  Types.Code = struct
  open Automaton
  module D = CodeGenDot.Make (S) (G) (A)

  let nterm_name n = (G.nterm n).ni_name.data

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
    Format.fprintf f "type token =\n";
    List.iter (write_term_cons f) infos
  ;;

  let write_entry f symbol =
    match (G.nterm symbol).ni_type with
    | None ->
      S.report_err ".mli backend requires all %%start symbol types to be specified"
    | Some ty ->
      Format.fprintf
        f
        "val %s : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> %t\n"
        (nterm_name symbol)
        (fun f -> write_string f ty)
  ;;

  let write f =
    let write_entry f (nt, _) = write_entry f nt in
    Format.fprintf
      f
      "%t%t\n%t\n%s"
      (fun f -> if S.compat then Format.fprintf f "%s\n" parsing_compat_mod)
      (fun f -> write_term_type f G.symbols)
      (fun f -> List.iter (write_entry f) A.automaton.a_starting)
      epilogue
  ;;

  let write () = write (Format.formatter_of_out_channel S.out)
end
