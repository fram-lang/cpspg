let lexbuf = Lexing.from_string ""

let lex tokens =
  let tokens = ref tokens in
  let next _ =
    match !tokens with
    | [] -> assert false
    | [ tok ] -> tok
    | tok :: toks ->
      tokens := toks;
      tok
  in
  next
;;

let check t parse name tok res =
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check t) name (parse (lex tok) lexbuf) res)
;;
