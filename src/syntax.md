This file describes the Surface Syntax and grammar of the toy language.

# Reserved Keywords
- `lambda` : Incanation of a lambda expression.
- `true`, `false` : Boolean literals.
<!-- - `if-then-else` : Ternary conditional operator. -->
<!-- - `first`: First element of a tuple. -->
<!-- - `let` : Variable declaration. -->
<!-- - `fn` : Function declaration. -->
<!-- - `print` : Print an expression. -->

## Surface Syntax
- `lambda <var>.<exprs>` : Lambda expression.
- `(e , e)` : Lambda expression application.
<!-- - `e ifthenelse e , e` : Ternary conditional operator. -->
<!-- - `{e , e , ...}` : Tuple|List expression. -->
<!-- - `e.first` : First element of a tuple. -->
<!-- - `e.<int n>` : Access the nth (zero based) element of a tuple. (evaluated at runtime) -->
<!-- - `let <var> = <expr>` : Variable declaration. -->
<!-- - `fn <function name> (<arg> , <arg> , ...) = <exprs>` : Function declaration. Syntactic sugar of lambda expressions and let expressions. No type checking or argument count checking rn. Function calls could also be some lambda expressions. Very similar to currying, just with confusing modern syntax. -->
<!-- - The program's file statement should not be terminated with any `;` -->

## Arithmetic Expressions
`<expr> + <expr>` : Addition
`<expr> - <expr>` : Subtraction
`<expr> * <expr>` : Multiplication
`<expr> / <expr>` : Division
`<expr> % <expr>` : Modulus