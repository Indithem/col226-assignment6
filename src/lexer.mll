{
    open Parser
}

let whitespace = [' ''\t''\n']
let alphanumerics = ['A'-'Z' 'a'-'z' '0'-'9']


rule lexer = parse
    | whitespace+ { lexer lexbuf }
    | "#" [^'\n']* { lexer lexbuf }

    | "lambda" { LAMBDA }

    | "ifthenelse" { IFTHENELSE }
    | "true" { BOOLCONST (true) }
    | "false" { BOOLCONST (false) }
    | "let" { LET }
    | "fn" { FN }
    | "print" { PRINT }

    | "first" { FIRST }
    
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "/" { DIVIDE }
    (* | "%" { MOD } *)

    | ['-']?['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }

    | "(" { LPAREN }
    | ")" { RPAREN }

    | "{" { LBRACE }
    | "}" { RBRACE }

    | ";" {SEP}
    | "." {DOT}
    | "," {COMMA}
    | "=" {EQUALS}

    | alphanumerics+ { VAR (Lexing.lexeme lexbuf) }

    | '"' [^'"']* '"' as s { STRING s }

    | eof {EOF}
    | _  {raise (Failure ("illegal character '" ^ Lexing.lexeme lexbuf ^ "' while lexing"))}

