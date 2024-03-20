{
    open Parser
}

let whitespace = [' ''\t''\n']
let alphanumerics = ['A'-'Z' 'a'-'z' '0'-'9']


rule lexer = parse
    | whitespace+ { lexer lexbuf }

    | "lambda" { LAMBDA }
    
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

    | alphanumerics+ { VAR (Lexing.lexeme lexbuf) }

    | eof {EOF}
    | _  {raise (Failure ("illegal character '" ^ Lexing.lexeme lexbuf ^ "' while lexing"))}

