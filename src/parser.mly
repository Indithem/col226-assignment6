%token LAMBDA
%token LPAREN RPAREN LBRACE RBRACE
%token SEP DOT EOF

%token<int> INT
%token<string> VAR
%token PLUS MINUS TIMES DIVIDE

%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Ast.expression list> main
%type <Ast.expression> expression

%%
main:
    | expression {[ $1 ]}
    | expression SEP main {$1::$3}
;

expression:
    | LBRACE expression RBRACE {$2}
    | INT {Ast.Int $1}
    | VAR {Ast.Var $1}
    | expression PLUS expression {Ast.Operation (Ast.Add, $1, $3)}
    | expression MINUS expression {Ast.Operation (Ast.Sub, $1, $3)}
    | expression TIMES expression {Ast.Operation (Ast.Mul, $1, $3)}
    | expression DIVIDE expression {Ast.Operation (Ast.Div, $1, $3)}

    | LAMBDA VAR DOT expression {Ast.Lambda ($2, $4)}
    | LPAREN expression expression RPAREN {Ast.Application ($2, $3)}
;


