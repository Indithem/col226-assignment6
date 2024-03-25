type inbuilt_operations = 
  | Add
  | Sub
  | Mul
  | Div

type inbuilt_const_types = 
  | Int of int
  | Bool of bool
  | String of string

  (**The abract syntax tree of the assignment *)
type expression = 
  | Const of inbuilt_const_types
  | Operation of inbuilt_operations * expression * expression
  | Ifthenelse of expression * expression * expression
  | Tuple of expression list
  | Project of expression * expression
  | Declaration of string * expression
  | Function of string * string list * expression list
  | Function_application of string * expression list
  | Print of expression

  | Var of string
  | Lambda of string * expression list
  | Application of expression * expression
;;
(** Operation Codes for the Stack machine *)
type opcode =
  | PUSH of inbuilt_const_types
  | POP
  | OPERATE of inbuilt_operations
  | IFTHENELSE of opcode list * opcode list
  | MAKE_TUPLE of opcode list list
  | PROJECT of opcode list
  | ASSIGN of string
  | PRINT

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
    | Lambda (s, e) -> [MAKE_CLOSURE (s, (List.concat_map compile e) @ [RETURN])]
    | Application (e1, e2) -> (compile e1) @ (compile e2) @ [APPLY_CLOSURE]
    | Ifthenelse (e1, e2, e3) -> (compile e1) @ [IFTHENELSE (compile e2, compile e3)]
    | Tuple es -> [MAKE_TUPLE (List.map compile es)]
    | Project (i, e) -> (compile e) @ [PROJECT (compile i)]
    | Declaration (s, e) -> (compile e) @ [ASSIGN s]
    | Function (x, vars, e) -> 
      let rec lambda_expr vars=
        match vars with 
          | [] -> Lambda ("", e)
          | [x] -> Lambda (x, e)
          | x::xs -> Lambda (x, [lambda_expr xs])
      in
      compile (Declaration(x, lambda_expr (List.rev vars)))
    | Function_application (f, es) ->
      [LOOKUP f] @ 
      (match es with | [] -> [PUSH (Int 0)] | _ -> List.concat_map compile es) 
      @ [APPLY_CLOSURE]
    | Print e -> (compile e) @ [PRINT]
;;

module StringMap = Map.Make(String)

type answers = 
  | Const of inbuilt_const_types
  | Tuple of answers list
  | VClosure of string * opcode list * answers StringMap.t
type table = answers StringMap.t
let bind x a t :table =
  StringMap.add x a t

(* Our SECD parameters *)
type environment = answers StringMap.t
type stack = answers list
type code = opcode list
type dump = (stack * environment * code) list

let print_gaps n =
  for i = 1 to n do
    Printf.printf "  "
  done;
  if n > 0 then Printf.printf "|__"
;;

let print_const_inbuilt c =
  match c with
    | Int i -> Printf.printf "Int: %d\n" i
    | Bool b -> Printf.printf "Bool: %b\n" b
    | String s -> Printf.printf "String: %s\n" s

let rec print_opcodes p (opcodes:opcode list) =
  let helper_printer opcode =
    print_gaps p;
    match opcode with
      | PUSH c ->  Printf.printf "PUSH ";print_const_inbuilt c
      | POP -> Printf.printf "POP\n"
      | OPERATE op ->
        let op_str = match op with
          | Add -> "Add"
          | Sub -> "Sub"
          | Mul -> "Mul"
          | Div -> "Div"
        in
        Printf.printf "OPERATION %s\n" op_str
      | LOOKUP x -> Printf.printf "LOOKUP Variable %s\n" x
      | MAKE_CLOSURE (x, ops) ->
        Printf.printf "CLOSURE with parameter %s in\n" x;
        print_opcodes (p+1) ops
      | RETURN -> Printf.printf "RETURN\n"
      | APPLY_CLOSURE -> Printf.printf "APPLY Closure\n"
      | IFTHENELSE (ops1, ops2) ->
        Printf.printf "IFTHENELSE IF:\n";
        print_opcodes (p+1) ops1;
        Printf.printf "ELSE:\n";
        print_opcodes (p+1) ops2
      | MAKE_TUPLE opslist ->
        Printf.printf "PUSH TUPLE LIST with %d elements\n" (List.length opslist);
        List.iter (fun ops -> print_opcodes (p+1) ops) opslist
      | PROJECT intOps ->
        Printf.printf "PROJECTION of result \n";
        print_opcodes (p+1) intOps
      | ASSIGN x -> Printf.printf "ASSIGN to Variable %s\n" x
      | PRINT -> Printf.printf "PRINT\n"
  in
  List.iter helper_printer opcodes

let rec print_answers p a =
  print_gaps p;
  match a with
    | Const c -> print_const_inbuilt c
    | VClosure(x, ops, env) ->
      Printf.printf "Value Closure with parameter %s in \n" x;
      print_opcodes (p+2) ops;
      print_gaps (p+1);
      Printf.printf "Environment:\n";
      StringMap.iter (fun k v -> print_gaps (p+2);Printf.printf "%s ->\n " k; print_answers (p+3) v) env
    | Tuple vs ->
      Printf.printf "Answer Tuple List with %d elements\n" (List.length vs);
      List.iter (fun v -> print_answers (p+1) v) vs

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
    
    | s, e, MAKE_TUPLE (cs)::c, d ->
      secd_machine 
        ((Tuple (List.map (fun c -> secd_machine [] e c [])
        cs))::s)
        e c d
    | Tuple vs::s, e, PROJECT i::c, d ->
      let v =
        let eval = secd_machine [] e i [] in
        let eval_int = match eval with
          | Const (Int i) -> i
          | _ -> raise (SECD_Exception (((s,e,c)::d),"Expected an integer for list indexing"))
        in
        try
          List.nth vs eval_int
        with _ -> raise (SECD_Exception (((s,e,c)::d),"Index out of bounds"))
      in
      secd_machine (v::s) e c d
    | a::s, e, ASSIGN x::c, d -> 
      secd_machine s (bind x a e) c d
    | a::s, e, PRINT::c, d -> 
      print_answers 0 a;
      secd_machine s e c d

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