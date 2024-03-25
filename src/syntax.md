This file describes the Surface Syntax and grammar of the toy language.

# Reserved Keywords
- `lambda` : Incanation of a lambda expression.
- `true`, `false` : Boolean literals.
- `if-then-else` : Ternary conditional operator.
- `first`: First element of a tuple.

## Surface Syntax
- `(e , e)` : Lambda expression application.
- `e ifthenelse e , e` : Ternary conditional operator.
- `{e , e , ...}` : Tuple|List expression.
- `e.first` : First element of a tuple.
- `e.<int n>` : Access the nth (zero based) element of a tuple. (evaluated at runtime)

# Grammar
## Lambda Expressions
`lambda <capture>.<body>` : A lambda expression.
`<capture>` captures a single variable.
`<body>` is the body of the lambda expression.

## Arithmetic Expressions
`<expr> + <expr>` : Addition
`<expr> - <expr>` : Subtraction
`<expr> * <expr>` : Multiplication
`<expr> / <expr>` : Division
`<expr> % <expr>` : Modulus