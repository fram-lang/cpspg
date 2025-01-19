let term =
  let pp _ _ = () in
  Alcotest.testable pp ( = )
;;

let _ =
  let open WhenParser in
  let check = Common.check term program in
  Alcotest.run
    "param"
    [ ( "when"
      , [ check
            "let a = a"
            [ KW_LET; IDENT "a"; EQ; IDENT "a"; EOF ]
            (TLet (PIdent "a", EIdent "a"))
        ; check
            "let _ = 1"
            [ KW_LET; WILDCARD; EQ; INT 1; EOF ]
            (TLet (PWildcard, EInt 1))
        ; check
            "let (a _) = (1 a)"
            [ KW_LET; LP; IDENT "a"; WILDCARD; RP; EQ; LP; INT 1; IDENT "a"; RP; EOF ]
            (TLet (PTuple [ PIdent "a"; PWildcard ], ETuple [ EInt 1; EIdent "a" ]))
        ] )
    ]
;;
