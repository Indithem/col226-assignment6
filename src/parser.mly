%{
    open Krivine;;
%}

%token LAMBDA
%token LPAREN RPAREN LBRACE RBRACE
%token SEP DOT EOF COMMA EQUALS

%token<int> INT
%token<string> VAR
%token<string> STRING
%token PLUS MINUS TIMES DIVIDE
%token<bool> BOOLCONST
%token IFTHENELSE FIRST LET FN PRINT

%left PLUS MINUS
%left TIMES DIVIDE

%start main
%type <Krivine.expression list> main

%%
main:
    | expression_body EOF {$1}
;

expression_body:
    | {[]}
    | expression {[$1]}
    | expression SEP expression_body {$1::$3}
;

expression:
    | LBRACE expression RBRACE {$2}
    | LBRACE list_expression RBRACE {Tuple $2}
    | FIRST LBRACE list_expression RBRACE {Project (Const(Int 0), Tuple($3))}
    | LBRACE list_expression RBRACE DOT expression {Project ($5, Tuple $2)}
    | BOOLCONST {Const (Bool $1)}
    | INT {Const (Int $1)}
    | STRING {Const (String $1)}
    | VAR {Var $1}
    | expression PLUS expression {Operation (Add, $1, $3)}
    | expression MINUS expression {Operation (Sub, $1, $3)}
    | expression TIMES expression {Operation (Mul, $1, $3)}
    | expression DIVIDE expression {Operation (Div, $1, $3)}
    | expression IFTHENELSE expression COMMA expression {Ifthenelse ($1, $3, $5)}
    | LET VAR EQUALS expression {Declaration ($2, $4)}
    // | function_def {$1} 
    // | VAR LPAREN list_expression RPAREN {Function_application ($1, $3)}
    // | PRINT expression {Print $2}

    | LAMBDA VAR DOT expression {Lambda ($2, $4)}
    | LPAREN expression COMMA expression RPAREN {Application ($2, $4)}
;

list_expression:
    | {[]}
    | expression {[$1]}
    | expression COMMA list_expression {$1::$3}
;

// function_def:
//     | FN VAR LPAREN vars_list RPAREN EQUALS LBRACE expression_body RBRACE {Function ($2, $4, $8)}

// vars_list:
//     | {[]}
//     | VAR {[$1]}
//     | VAR COMMA vars_list {$1::$3}

