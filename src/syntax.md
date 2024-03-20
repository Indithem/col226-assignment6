This file describes the Surface Syntax and grammar of the toy language.

# Reserved Keywords
- `lambda` : Incanation of a lambda expression.

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