type inbuilt_operations = 
  | Add
  | Sub
  | Mul
  | Div

type inbuilt_const_types = 
  | Int of int
  | Bool of bool

  (**The abract syntax tree of the assignment *)
type expression = 
  | Const of inbuilt_const_types
  | Operation of inbuilt_operations * expression * expression
  | Ifthenelse of expression * expression * expression

  | Var of string
  | Lambda of string * expression
  | Application of expression * expression
;;
(** Operation Codes for the Stack machine *)
type opcode =
  | PUSH of inbuilt_const_types
  | POP
  | OPERATE of inbuilt_operations
  | IFTHENELSE of opcode list * opcode list

  | LOOKUP of string
  | MAKE_CLOSURE of string * opcode list 
  | RETURN
  | APPLY_CLOSURE
;;
  
(** Compiling the expression into a list of opcodes *)
let rec compile e =
  match e with
    | Const i -> [PUSH i]
    | Operation (op, e1, e2) -> (compile e1) @ (compile e2) @ [OPERATE op]
    
    | Var s -> [LOOKUP s]
    | Lambda (s, e) -> [MAKE_CLOSURE (s, compile e @ [RETURN])]
    | Application (e1, e2) -> (compile e1) @ (compile e2) @ [APPLY_CLOSURE]
    | Ifthenelse (e1, e2, e3) -> (compile e1) @ [IFTHENELSE (compile e2, compile e3)]
    
;;

type todo = unit;;
module StringMap = Map.Make(String)

type answers = 
  | Const of inbuilt_const_types
  | VClosure of string * opcode list * answers StringMap.t
type table = answers StringMap.t
let bind x a t :table =
  StringMap.add x a t

(* Our SECD parameters *)
type environment = answers StringMap.t
type stack = answers list
type code = opcode list
type dump = (stack * environment * code) list

exception SECD_Exception of dump * string
let rec secd_machine (s:stack) (e:environment) (c:code) (d:dump) =
  match (s, e, c, d) with 
    | (v::_, _, [], _) -> v
    | s, e, PUSH i ::c, d -> 
      secd_machine (Const i :: s) e c d
    | v::s, e, POP::c, d -> 
      secd_machine s e c d
    | Const (Int i2)::Const (Int i1)::s, e, OPERATE op::c, d -> 
      let v = Const (Int (
        match op with
          | Add -> i1 + i2
          | Sub -> i1 - i2
          | Mul -> i1 * i2
          | Div -> i1 / i2
          | _ -> raise (SECD_Exception (((s,e,c)::d),"Expected an integer operation"))
      ))
      in
      secd_machine (v::s) e c d
    | Const (Bool b)::s, e, IFTHENELSE (c1, c2)::c, d ->
      secd_machine s e ((if b then c1 else c2) @ c) d
    | _, _, IFTHENELSE (_)::_, _ -> raise (SECD_Exception (((s,e,c)::d),"Expected a boolean value"))
    
    | s, e, LOOKUP x::c, d -> 
      let v = 
        try StringMap.find x e 
        with Not_found -> raise (SECD_Exception (((s,e,c)::d),"Variable not found"))
      in
      secd_machine (v::s) e c d
    | s, e, MAKE_CLOSURE(x, c1)::c2, d ->
      let v = VClosure(x, c1, e) in
      secd_machine (v::s) e c2 d
    | a::VClosure (x,c',e')::s, e, APPLY_CLOSURE::c, d ->
      secd_machine [] (bind x a e') c' ((s,e,c)::d)
    | a::_, _, RETURN::_, (s,e,c)::d ->
      secd_machine (a::s) e c d

    | _ -> raise (SECD_Exception (((s,e,c)::d),"Unknown SECD machine state"))