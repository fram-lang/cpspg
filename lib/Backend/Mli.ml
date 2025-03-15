let dummy_span = Lexing.dummy_pos, Lexing.dummy_pos
let verbatim data = { Raw.span = dummy_span; data = String.trim data }

let parsing_compat_mod = {|
module Parsing : module type of Stdlib.Parsing
|} |> verbatim
[@@ocamlformat "disable"]

let epilogue = {|
val error_token : unit -> token option
val expected_tokens : unit -> string list
|} |> verbatim
[@@ocamlformat "disable"]

module Make (S : Types.BackEndSettings) (G : Types.Grammar) (A : Types.Automaton) :
  Types.Code = struct
  open Automaton
  module PP = Printer.Ml (S)
  module D = Dot.Make (S) (G) (A)

  (* Utils *)

  let nterm_name n = (G.nterm n).ni_name.data

  (* Rest *)

  let make_token_type symbols =
    let get_info = function
      | NTerm _ -> None
      | Term t -> Some (G.term t)
    and cmp a b = String.compare a.ti_name.data b.ti_name.data in
    let infos = List.filter_map get_info symbols |> List.sort cmp in
    List.map (fun i -> { PP.name = i.ti_name; contents = i.ti_ty }) infos
  ;;

  let make_entry symbol =
    match (G.nterm symbol).ni_type with
    | None ->
      S.report_err ".mli backend requires all %%start symbol types to be specified";
      let s =
        Printf.sprintf
          "val %s : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> unit\n"
          (nterm_name symbol)
      in
      PP.StructVerbatim (verbatim s)
    | Some ty ->
      let s =
        Printf.sprintf
          "val %s : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> %s\n"
          (nterm_name symbol)
          ty.data
      in
      PP.StructVerbatim (verbatim s)
  ;;

  let make_file { a_starting; _ } =
    let starting = List.map (fun (sym, _) -> make_entry sym) a_starting in
    []
    @ [ PP.StructType ("token", make_token_type G.symbols) ]
    @ (if S.compat then [ PP.StructVerbatim parsing_compat_mod ] else [])
    @ [ PP.StructVerbatim epilogue ]
    @ starting
  ;;

  let write () = make_file A.automaton |> PP.pp_structures
end
