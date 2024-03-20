%{
    open Secd;;
%}

%token LAMBDA
%token LPAREN RPAREN LBRACE RBRACE
%token SEP DOT EOF

%token<int> INT
%token<string> VAR
%token PLUS MINUS TIMES DIVIDE

%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Secd.expression list> main

%%
main:
    | expression {[ $1 ]}
    | expression SEP main {$1::$3}
;

expression:
    | LBRACE expression RBRACE {$2}
    | INT {Int $1}
    | VAR {Var $1}
    | expression PLUS expression {Operation (Add, $1, $3)}
    | expression MINUS expression {Operation (Sub, $1, $3)}
    | expression TIMES expression {Operation (Mul, $1, $3)}
    | expression DIVIDE expression {Operation (Div, $1, $3)}

    | LAMBDA VAR DOT expression {Lambda ($2, $4)}
    | LPAREN expression expression RPAREN {Application ($2, $3)}
;


