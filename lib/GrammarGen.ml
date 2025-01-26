module IntMap = Map.Make (Int)
module TermMap = Map.Make (Automaton.Terminal)
module NTermMap = Map.Make (Automaton.Nonterminal)

module SymMap = Map.Make (struct
    type t = Raw.symbol

    let compare = compare
  end)

type value =
  | VDummy (* error value *)
  | VSymbol of Automaton.symbol (* ordinary symbol *)
  | VInline of Automaton.item list (* %inline symbol *)
  | VRule of Raw.rule (* higher order rule. *)

(** Standard library, copied from [Standard.mly] file into [Standard.ml] by dune *)
module Std = struct
  let lexbuf = Lexing.from_string Standard.contents
  let _ = Lexing.set_filename lexbuf "<standard.mly>"
  let raw = Parser.grammar Lexer.main lexbuf
end

(** [equal] and [hash] functions that compare rules by their ids only *)
module Rule = struct
  type t = Raw.rule

  let equal r1 r2 = r1.Raw.r_id.data == r2.Raw.r_id.data
  let hash r = Hashtbl.hash r.Raw.r_id.data
end

(** [equal] and [hash] function that compare [value]s using [Rule] module above *)
module Value = struct
  type t = value

  let equal v1 v2 =
    match v1, v2 with
    | VDummy, VDummy -> true
    | VSymbol s1, VSymbol s2 -> s1 = s2
    | VInline i1, VInline i2 -> i1 = i2
    | VRule r1, VRule r2 -> Rule.equal r1 r2
    | _, _ -> false
  ;;

  let hash = function
    | VDummy -> Hashtbl.hash 0
    | VSymbol s -> Hashtbl.hash (1, s)
    | VInline i -> Hashtbl.hash (2, i)
    | VRule r -> Hashtbl.hash (3, Rule.hash r)
  ;;
end

(** Specialized hashmap that is used to store rule instances, indexed by rule and its arguments.
    This map uses [Rule] and [Value] modules from above, which simplifies grammar generation while
    comparing rules only by their ids *)
module InstanceMap = Hashtbl.Make (struct
    type t = Raw.rule * value list

    let equal (r1, v1) (r2, v2) = Rule.equal r1 r2 && List.equal Value.equal v1 v2
    let hash (r, vs) = Hashtbl.hash (Rule.hash r, List.map Value.hash vs)
  end)

module Run (S : Types.Settings) (R : Types.Raw) : Types.Grammar = struct
  open Raw
  open Automaton

  let nterm_id = InstanceMap.create 128
  let nterm_info = Hashtbl.create 128
  let actions = Hashtbl.create 128

  let sym_name = function
    | Raw.Term t -> t
    | Raw.NTerm n -> n
  ;;

  let header =
    let get_code = function
      | Raw.DeclCode code -> Some code
      | _ -> None
    in
    List.filter_map get_code R.raw.r_decls
  ;;

  (** [prec] is a mapping from precedence names to their [(left, right)] levels.
      Left-associative operators are represented with precedence levels [(p + 1, p)],
      right-associative operators with levels [(p, p + 1)], and non-associative operators
      with levels [(p, p)]. This encoding allows for straightforward comparison of precedence
      levels by comparing the left and right values, without needing to handle associativity separately. *)
  let prec =
    let prec = Hashtbl.create 64 in
    let iter_sym p sym =
      if Hashtbl.mem prec sym.data
      then S.report_warn ~loc:sym.loc "duplicate precedence %s" sym.data;
      Hashtbl.replace prec sym.data p
    in
    let iter i = function
      | Raw.DeclLeft xs -> List.iter (iter_sym ((i * 2) + 1, i * 2)) xs
      | Raw.DeclRight xs -> List.iter (iter_sym (i * 2, (i * 2) + 1)) xs
      | Raw.DeclNonassoc xs -> List.iter (iter_sym (i * 2, i * 2)) xs
      | _ -> ()
    in
    List.iteri iter R.raw.r_decls;
    prec
  ;;

  let types =
    let types = Hashtbl.create 64 in
    let add_type typ id =
      match Hashtbl.find_opt types id.data with
      | Some ty when ty <> typ ->
        S.report_err ~loc:id.loc "conflicting types for symbol %s" (sym_name id.data)
      | Some _ ->
        S.report_warn ~loc:id.loc "multiple types for symbol %s" (sym_name id.data)
      | _ -> Hashtbl.add types id.data typ
    in
    let add_nterm_type typ s = add_type typ { s with data = NTerm s.data } in
    let iter = function
      | Raw.DeclType (ty, ids) -> List.iter (add_type ty) ids
      | Raw.DeclStart (Some ty, ids) -> List.iter (add_nterm_type ty) ids
      | _ -> ()
    in
    List.iter iter R.raw.r_decls;
    types
  ;;

  (** [term] is a mapping from a terminal name to its id and info. *)
  let term =
    let term = Hashtbl.create 128 in
    let iter_token ty name =
      let prec = Hashtbl.find_opt prec name.data in
      let info = { ti_name = name; ti_ty = ty; ti_prec = prec } in
      if Hashtbl.mem term name.data
      then S.report_warn ~loc:name.loc "duplicate terminal symbol %s" name.data
      else Hashtbl.add term name.data (Hashtbl.length term |> Terminal.of_int, info)
    in
    let iter_decl = function
      | Raw.DeclToken (ty, ids) -> List.iter (iter_token ty) ids
      | _ -> ()
    in
    List.iter iter_decl R.raw.r_decls;
    term
  ;;

  (** All known rules, both from given grammar and the standard library *)
  let rules =
    let rules = Hashtbl.create 64 in
    let iter_rule rule =
      let name = rule.r_id in
      if Hashtbl.mem rules name.data
      then S.report_warn ~loc:name.loc "duplicate rule %s" name.data
      else Hashtbl.add rules name.data rule
    and iter_std rule =
      let name = rule.r_id in
      if not (Hashtbl.mem rules name.data) then Hashtbl.add rules name.data rule
    in
    List.iter iter_rule R.raw.r_rules;
    List.iter iter_std Std.raw.r_rules;
    rules
  ;;

  let init_env loc params given =
    let rec aux env = function
      | [], [] -> env
      | [], _ ->
        S.report_err ~loc "too many arguments";
        env
      | params, [] ->
        S.report_err ~loc "not enough arguments";
        List.fold_left (fun env p -> SymMap.add p.data VDummy env) env params
      | p :: params, a :: given ->
        let env = SymMap.add p.data a env in
        aux env (params, given)
    in
    aux SymMap.empty (params, given)
  ;;

  let tr_term name =
    match Hashtbl.find_opt term name.data with
    | Some (t, _) -> t
    | None ->
      S.report_err ~loc:name.loc "unknown terminal symbol %s" name.data;
      Terminal.dummy
  ;;

  let tr_action prod code =
    let get_args = function
      | { p_id = None; _ } -> None
      | { p_id = Some id; _ } -> Some id.data
    in
    let args = List.map get_args prod in
    match Hashtbl.find_opt actions (code, args) with
    | Some (id, _) -> id
    | None ->
      let action = { sa_args = args; sa_code = code }
      and id = Hashtbl.length actions in
      Hashtbl.add actions (code, args) (id, action);
      id
  ;;

  let rec tr_actions env prod actions =
    let get symbol =
      match tr_actual env { a_symbol = symbol; a_args = [] } with
      | VInline _ ->
        S.report_err ~loc:symbol.loc "inline symbols are not allowed in conditions";
        None
      | VDummy -> None
      | value -> Some value
    in
    let f = function
      | { a_cond = None; a_code = code } -> Some (tr_action prod code)
      | { a_cond = Some (lhs, rhs); a_code = code } ->
        (match get lhs, get rhs with
         | Some l, Some r when l = r -> Some (tr_action prod code)
         | _ -> None)
    in
    List.find_map f actions

  and tr_values values =
    let append_one x xxs = List.map (fun xs -> x :: xs) xxs
    and append_some yys xxs =
      List.map (fun ys -> List.map (List.rev_append ys) xxs) yys |> List.flatten
    in
    let f (sym, arg) = function
      | VDummy ->
        let s = Term Terminal.dummy in
        append_one s sym, append_one None arg
      | VRule _ ->
        let s = Term Terminal.dummy in
        append_one s sym, append_one None arg
      | VSymbol s -> append_one s sym, append_one None arg
      | VInline items ->
        let s = List.map (fun i -> i.i_suffix) items
        and a = List.map (fun i -> [ i.i_action ]) items in
        append_some s sym, append_some a arg
    in
    let sym, arg = List.fold_left f ([ [] ], [ [] ]) values in
    List.map List.rev sym, List.map List.rev arg

  and tr_actual env actual : value =
    let sym = actual.a_symbol in
    let get_nterm id args =
      match Hashtbl.find_opt rules id.data with
      | None ->
        S.report_err ~loc:id.loc "unknown nonterminal symbol %s" id.data;
        VDummy
      | Some rule when args = [] && rule.r_params <> [] -> VRule rule
      | Some rule -> tr_args env actual.a_args |> instantiate rule
    in
    match actual.a_symbol.data, SymMap.find_opt sym.data env with
    | _, Some (VRule rule) when actual.a_args <> [] ->
      instantiate rule (tr_args env actual.a_args)
    | _, Some value ->
      if actual.a_args <> []
      then S.report_err ~loc:sym.loc "this value does not accept arguments";
      value
    | Raw.Term t, None ->
      if actual.a_args <> []
      then S.report_err ~loc:sym.loc "terminal symbols do not accept arguments";
      VSymbol (Term (tr_term { sym with data = t }))
    | Raw.NTerm id, None -> get_nterm { sym with data = id } actual.a_args

  and tr_production env prod =
    let values = List.map (fun p -> tr_actual env p.p_actual) prod.p_prod in
    let get_prec p = Hashtbl.find_opt prec p.data in
    let get_group suffix args =
      match tr_actions env prod.p_prod prod.p_actions with
      | Some action ->
        let action = Some { ac_id = action; ac_args = args }
        and prec = Option.bind prod.p_prec get_prec in
        Some { i_suffix = suffix; i_action = action; i_prec = prec }
      | None -> None
    in
    let sym, arg = tr_values values in
    List.map2 get_group sym arg |> List.filter_map Fun.id

  and tr_args env args =
    let tr_arg = function
      | Raw.Arg actual -> tr_actual env actual
      | Raw.ArgInline { a_prod; a_action } ->
        let p_actions = [ { a_cond = None; a_code = a_action } ] in
        VInline (tr_production env { p_prod = a_prod; p_actions; p_prec = None })
    in
    List.map tr_arg args

  and instantiate rule args =
    let compare_item_len a b = -List.compare_lengths a.i_suffix b.i_suffix in
    match InstanceMap.find_opt nterm_id (rule, args) with
    | Some id ->
      if rule.r_inline
      then VInline (Hashtbl.find nterm_info id |> snd).g_items
      else VSymbol (NTerm id)
    | None ->
      let id = InstanceMap.length nterm_id |> Nonterminal.of_int in
      InstanceMap.add nterm_id (rule, args) id;
      let env = init_env rule.r_id.loc rule.r_params args in
      let items = List.map (tr_production env) rule.r_prods |> List.flatten in
      let info =
        { ni_name = rule.r_id
        ; ni_starting = false
        ; ni_type = Hashtbl.find_opt types (NTerm rule.r_id.data)
        }
      and group =
        { g_symbol = id
        ; g_prefix = []
        ; g_items = List.sort compare_item_len items
        ; g_lookahead = TermSet.empty
        ; g_starting = false
        }
      in
      Hashtbl.add nterm_info id (info, group);
      if rule.r_inline then VInline group.g_items else VSymbol (NTerm id)
  ;;

  (* Find all starting points and fill [nterm_id] and [nterm_info] tables *)
  let _ =
    let start name rule =
      match instantiate rule [] with
      | VDummy | VSymbol (Term _) ->
        S.report_err ~loc:name.loc "starting rule %s is invalid" name.data
      | VRule _ ->
        S.report_err ~loc:name.loc "starting rule %s cannot accept parameters" name.data
      | VInline _ ->
        S.report_err ~loc:name.loc "starting rule %s cannot be inline" name.data
      | VSymbol (NTerm id) ->
        let info, group = Hashtbl.find nterm_info id in
        if info.ni_starting
        then S.report_warn ~loc:name.loc "duplicate start declaration of %s" name.data
        else Hashtbl.replace nterm_info id ({ info with ni_starting = true }, group)
    in
    let iter_start name =
      match Hashtbl.find_opt rules name.data with
      | None -> S.report_err ~loc:name.loc "unknown starting symbol %s" name.data
      | Some rule -> start name rule
    in
    let iter_decl = function
      | Raw.DeclStart (_, ids) -> List.iter iter_start ids
      | _ -> ()
    in
    List.iter iter_decl R.raw.r_decls
  ;;

  let symbols =
    let term = Hashtbl.to_seq_values term |> Seq.map (fun (t, _) -> Term t)
    and nterm = InstanceMap.to_seq_values nterm_id |> Seq.map (fun n -> NTerm n) in
    Seq.append term nterm |> List.of_seq |> List.sort compare
  ;;

  let term =
    let term = Hashtbl.to_seq_values term |> TermMap.of_seq in
    fun t -> TermMap.find t term
  ;;

  (* Collect all non-terminals and groups, also attach missing precedence levels to items. *)
  let nterm, group =
    let symbol_prec = function
      | Term t when t = Terminal.dummy -> None
      | Term t -> (term t).ti_prec
      | NTerm _ -> None
    in
    let attach_prec item =
      let prec =
        match item.i_prec with
        | Some prec -> Some prec
        | None -> List.find_map symbol_prec item.i_suffix
      in
      { item with i_prec = prec }
    in
    let attach_precs (id, (info, group)) =
      id, (info, { group with g_items = List.map attach_prec group.g_items })
    in
    let nterm = Hashtbl.to_seq nterm_info |> Seq.map attach_precs |> NTermMap.of_seq in
    (fun n -> NTermMap.find n nterm |> fst), fun n -> NTermMap.find n nterm |> snd
  ;;

  let actions = Hashtbl.to_seq_values actions |> IntMap.of_seq
end
