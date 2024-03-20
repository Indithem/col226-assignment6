type inbuilt_operations = 
  | Add
  | Sub
  | Mul
  | Div

type expression = 
  | Int of int
  | Operation of inbuilt_operations * expression * expression

  | Var of string
  | Lambda of string * expression
  | Application of expression * expression
  