let rec iter_sep f sep = function
  | [] -> ()
  | [ x ] -> f x
  | x :: xs ->
    f x;
    sep ();
    iter_sep f sep xs
;;

module type Output = sig
  val out : out_channel
end

module Make (O : Output) = struct
  let indentation = ref 0
  let at_newline = ref false
  let line_number = ref 1

  let output_string_raw s =
    let n = String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 0 s in
    line_number := !line_number + n;
    at_newline := false;
    output_string O.out s
  ;;

  let print_newline () =
    output_char O.out '\n';
    incr line_number;
    at_newline := true
  ;;

  let output_single_line s i = function
    | 0 -> ()
    | l ->
      if !at_newline then output_substring O.out "            " 0 !indentation;
      at_newline := false;
      output_substring O.out s i l
  ;;

  let output_single_line_end s i l =
    output_single_line s i l;
    output
  ;;

  let rec output_substring s i l =
    match String.index_from_opt s i '\n' with
    | None -> output_single_line s i l
    | Some j when j >= i + l -> output_single_line s i l
    | Some j ->
      output_single_line s i (j - i);
      print_newline ();
      output_substring s (j + 1) (l - (j + 1 - i))
  ;;

  let output_string s = output_substring s 0 (String.length s)

  let output_char = function
    | '\n' -> print_newline ()
    | c -> output_char O.out c
  ;;

  let indented f arg =
    indentation := !indentation + 2;
    let r = f arg in
    indentation := !indentation - 2;
    r
  ;;

  let printf fmt = Printf.ksprintf output_string fmt
end

module Ml (S : Types.BackEndSettings) = struct
  include Make (S)

  type recursive =
    | Recursive
    | NonRecursive

  type expr =
    | ExprUnit
    | ExprId of string
    | ExprCall of string * expr list
    | ExprLabeled of string
    | ExprGrouped of expr
    | ExprSeq of expr list
    | ExprLet of recursive * binding list * expr
    | ExprMatch of expr * case list
    | ExprVerbatim of string Raw.node list

  and binding =
    { name : string
    ; args : expr list
    ; expr : expr
    ; comment : string option
    }

  and case =
    { patterns : (string * string option) list
    ; cexpr : expr
    ; ccomment : string option
    }

  type constructor =
    { name : string Raw.node
    ; contents : string Raw.node option
    }

  type structure =
    | StructVerbatim of string Raw.node
    | StructType of string * constructor list
    | StructLet of recursive * bool * binding list
    | StructModule of string * structure list

  let pp_line_directive f l c =
    if not !at_newline then print_newline ();
    Printf.sprintf "# %d \"%s\"\n%s" l f (String.make c ' ') |> output_string_raw
  ;;

  let pp_string_node { Raw.span = loc, _; data } =
    match loc with
    | _ when not S.line_directives -> output_string (String.trim data)
    | loc when loc = Lexing.dummy_pos -> output_string data
    | loc ->
      pp_line_directive loc.pos_fname loc.pos_lnum (loc.pos_cnum - loc.pos_bol);
      output_string_raw data;
      pp_line_directive S.name (!line_number + 2) 0
  ;;

  let pp_comment = function
    | None -> ()
    | Some c ->
      output_string "(*";
      indented output_string c;
      output_string "*)\n"
  ;;

  let rec pp_expr = function
    | ExprUnit -> output_string "()"
    | ExprId s -> output_string s
    | ExprCall (callee, args) ->
      let rest expr =
        output_string " ";
        pp_expr expr
      in
      output_string callee;
      List.iter rest args
    | ExprLabeled s -> printf "~%s" s
    | ExprGrouped expr ->
      output_char '(';
      pp_expr expr;
      output_char ')'
    | ExprSeq [] -> assert false
    | ExprSeq (expr :: exprs) ->
      let rest expr =
        output_string ";\n";
        pp_expr expr
      in
      pp_expr expr;
      List.iter rest exprs
    | ExprLet (_, [], expr) -> pp_expr expr
    | ExprLet (recursive, bindings, expr) ->
      (match recursive with
       | Recursive -> output_string "let rec "
       | NonRecursive -> output_string "let ");
      iter_sep (pp_binding false) (fun () -> output_string "\nand ") bindings;
      output_string " in\n";
      pp_expr expr
    | ExprMatch (expr, cases) ->
      output_string "match ";
      pp_expr expr;
      output_string " with\n";
      iter_sep pp_case print_newline cases
    | ExprVerbatim data -> List.iter pp_string_node data

  and pp_binding bl { name; args; expr; comment = _ } =
    let pp_arg a =
      output_char ' ';
      pp_expr a
    and pp_block_expr expr =
      print_newline ();
      indented pp_expr expr;
      print_newline ()
    in
    output_string name;
    List.iter pp_arg args;
    output_string " = ";
    if bl then pp_block_expr expr else pp_expr expr

  and pp_case { patterns; cexpr; ccomment } =
    let pp_pattern = function
      | name, None -> printf "| %s " name
      | name, Some arg -> printf "| %s %s " name arg
    in
    pp_comment ccomment;
    List.iter pp_pattern patterns;
    output_string "->\n";
    indented pp_expr cexpr
  ;;

  let pp_constructor { name; contents } =
    output_string "| ";
    pp_string_node name;
    match contents with
    | None -> print_newline ()
    | Some c ->
      output_string " of (";
      pp_string_node c;
      output_string ")\n"
  ;;

  let rec pp_structure = function
    | StructVerbatim text ->
      pp_string_node text;
      if not S.line_directives then print_newline ()
    | StructType (name, constructors) ->
      printf "type %s =\n" name;
      indented (List.iter pp_constructor) constructors;
      printf ";;\n"
    | StructLet (_, _, []) -> assert false
    | StructLet (recursive, compact, binding :: bindings) ->
      let pp_binding b1 b2 binding =
        output_string b1;
        pp_comment binding.comment;
        output_string b2;
        pp_binding (not compact) binding
      in
      let before = if recursive = Recursive then "let rec " else "let " in
      pp_binding "" before binding;
      List.iter (pp_binding "\n" "and ") bindings;
      if compact then output_string "\n" else output_string ";;\n"
    | StructModule (name, contents) ->
      printf "module %s = struct\n" name;
      indented pp_structures contents;
      output_string "end\n"

  and pp_structures xs = iter_sep pp_structure print_newline xs
end
