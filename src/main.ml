let print_gaps n =
  for i = 1 to n do
    Printf.printf "  "
  done;
  if n > 0 then Printf.printf "|__"
;;
open Secd;;
let print_const_inbuilt c =
  match c with
    | Int i -> Printf.printf "Int: %d\n" i
    | Bool b -> Printf.printf "Bool: %b\n" b
let rec print_ast p (ast:expression)=
  print_gaps p;
  match ast with
  | Const c -> print_const_inbuilt c
  | Var x -> Printf.printf "Variable: %s\n" x
  | Operation (op, e1, e2) ->
    let op_str = match op with
      | Add -> "Add"
      | Sub -> "Sub"
      | Mul -> "Mul"
      | Div -> "Div"
    in
    Printf.printf "Operation: %s\n" op_str;
    print_ast (p+1) e1;
    print_ast (p+1) e2
  | Lambda (x, e) ->
    Printf.printf "Lambda: with parameter %s\n" x;
    print_ast (p+1) e
  | Application (e1, e2) ->
    Printf.printf "Application:\n";
    print_ast (p+1) e1;
    print_ast (p+1) e2
  | Ifthenelse (e1, e2, e3) ->
    Printf.printf "If:\n";
    print_ast (p+1) e1;
    Printf.printf "Then:\n";
    print_ast (p+2) e2;
    Printf.printf "Else:\n";
    print_ast (p+2) e3
  | Tuple es ->
    Printf.printf "Tuple List:\n";
    List.iter (fun e -> print_ast (p+1) e) es
  | Project (i, e) ->
    Printf.printf "Projection of {}th element evaluated by \n";
    print_ast (p+1) i;
    Printf.printf "from the tuple:\n";
    print_ast (p+1) e
;;

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
      | APPLY_CLOSURE -> Printf.printf "APPLY\n"
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

let print_environment env =
  StringMap.iter (fun k v -> Printf.printf "%s -> " k; print_answers 0 v) env

let print_stack stack =
  List.iter (print_answers 0) stack

let print_code ops =
  print_opcodes 0 ops

let print_dump dump = 
  List.iter
  (
    fun (stack, env, ops) ->
      Printf.printf "Stack:\n";
      print_stack stack;
      Printf.printf "Environment:\n";
      print_environment env;
      Printf.printf "Code:\n";
      print_code ops
  )
  dump

let main expr =
  Printf.printf "\x1B[33mAst:\n";
  print_ast 0 expr;
  let ops = compile expr in
  Printf.printf "\x1B[34mOp codes:\n";
  print_code ops;
  try
    let result = secd_machine [] StringMap.empty ops [] in
    Printf.printf "\x1B[32mResult:\n";
    print_answers 0 result;
  with
    | SECD_Exception (dmp, msg) ->
      Printf.printf "\x1B[31mSECD Execution error: %s\n" msg;
      print_dump dmp;
    ;
  Printf.printf "\x1B[00m";
;;

let lexbufr = Lexing.from_channel stdin in
try
let expressions = Parser.main Lexer.lexer lexbufr in
Printf.printf "Parsed %d statements\n" (List.length expressions);
List.iter main expressions;
with 
  Stdlib.Parsing.Parse_error ->
  begin
    let curr = lexbufr.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbufr in
    (* let tail = Sql_lexer.ruleTail "" lexbufr in *)
    Printf.printf "\x1B[31mSyntax error at line %d, character %d, token %s\x1B[00m\n" line cnum tok;
  end