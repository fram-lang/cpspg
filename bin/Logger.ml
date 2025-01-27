module IntSet = Set.Make (Int)

type conflict = int * Cpspg.Automaton.Terminal.t * Cpspg.Automaton.action list

let conflicts : conflict list ref = ref []
let has_error = ref false

let report ?loc tag msg =
  (match loc with
   | Some ({ Lexing.pos_fname = f; pos_lnum = l; pos_cnum = c; pos_bol = b }, _) ->
     Format.eprintf "\027[1mFile \"%s\", line %d, character %d\027[0m:\n" f l (c - b + 1)
   | None -> ());
  Format.eprintf "%s: %t\n\n%!" tag msg
;;

let report_err ?loc =
  has_error := true;
  Format.kdprintf (report ?loc "\027[1;31mError\027[0m")
;;

let report_warn ?loc =
  has_error := !has_error;
  Format.kdprintf (report ?loc "\027[1;35mWarning\027[0m")
;;

let report_conflict id sym actions =
  has_error := true;
  conflicts := (id, sym, actions) :: !conflicts
;;

let has_error () = !has_error

module Conflict (G : Cpspg.Types.Grammar) (A : Cpspg.Types.Automaton) = struct
  open Cpspg.Automaton

  let pp_sym f = function
    | Term t -> Format.fprintf f " %s" (G.term t).ti_name.data
    | NTerm n -> Format.fprintf f " %s" (G.nterm n).ni_name.data
  ;;

  let pp_reduction f g =
    Format.fprintf
      f
      "%t ->%t"
      (fun f -> pp_sym f (NTerm g.g_symbol))
      (fun f -> List.rev g.g_prefix |> List.iter (pp_sym f))
  ;;

  let pp_action f id = function
    | Shift -> Format.fprintf f "\n  - shift"
    | Reduce (i, _) ->
      let state = IntMap.find id A.automaton.a_states in
      let group = List.nth (state.s_kernel @ state.s_closure) i in
      Format.fprintf f "\n  - reduce%t" (fun f -> pp_reduction f group)
  ;;

  let pp_error f id sym actions =
    Format.fprintf
      f
      "Conflict in state %d on symbol%t:%t"
      id
      (fun f -> pp_sym f (Term sym))
      (fun f -> List.iter (pp_action f id) actions)
  ;;

  let report conflicts =
    let aux (id, sym, actions) = report_err "%t" (fun f -> pp_error f id sym actions) in
    List.iter aux conflicts
  ;;

  let _ = report !conflicts
end
