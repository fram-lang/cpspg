# Fram tools for cpspg
The files `Parsing.fram` and `Utils.fram` should be placed in the same
directory as the generated parser.

## Utils
This module contains functions and types that should be part of Fram's
standard library, but which were not ready when the Fram adaptation
of cpspg was created. It should eventually fall out of use.

## Parsing
This module contains the definition of position type and of the `Lex`
effect, used by the generated parser and handled by the written
lexer. It also defines the `Error` effect and the function `error`
that the user should use for reporting errors within semantic actions.

