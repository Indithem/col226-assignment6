# SECD Machine
This is an implementation of SECD machine for a simple toy language in OCaml.

## Key Points
- The SECD machine implements Call-by Value semantics. (**not** lazy evaluation)
- All results/answers are actually closures.
    * This means that any constant is actually a constant lambda expression with an null environment.


# Compiler
A compiler that takes human readable code (the toy language) and compiles it into opcodes of the SECD machine.

# Parser and Lexer
A parser and lexer are made using ocamllex and ocamlyacc. These parse the toy language.

# Toy Language Syntax
[see this file](src/syntax.md)