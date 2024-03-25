let print_gaps n =
  for i = 1 to n do
    Printf.printf "  "
  done;
  if n > 0 then Printf.printf "|__"
;;
open Secd;;
let rec print_ast p (ast:expression)=
  print_gaps p;
  match ast with
  | Int i -> Printf.printf "Int: %d\n" i
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
;;

let rec print_opcodes p (opcodes:opcode list) =
  let helper_printer opcode =
    print_gaps p;
    match opcode with
      | PUSH i -> Printf.printf "PUSH %d\n" i
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
  in
  List.iter helper_printer opcodes

let rec print_answers p a =
  match a with
    | Int i -> 
      print_gaps p;
      Printf.printf "Int: %d\n" i;
    | VClosure(x, ops, env) ->
      print_gaps p;
      Printf.printf "Value Closure with parameter %s in \n" x;
      print_opcodes 1 ops;
      print_gaps (p+1);
      Printf.printf "Environment:\n";
      StringMap.iter (fun k v -> print_gaps (p+2);Printf.printf "%s ->\n " k; print_answers (p+3) v) env

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
  Printf.printf "Ast:\n";
  print_ast 0 expr;
  let ops = compile expr in
  Printf.printf "Op codes:\n";
  print_code ops;
  try
    let result = secd_machine [] StringMap.empty ops [] in
    Printf.printf "Result:\n";
    print_answers 0 result
  with
    | SECD_Exception (dmp, msg) ->
      Printf.printf "SECD Execution error: %s\n" msg;
      print_dump dmp
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
    Printf.printf "Syntax error at line %d, character %d, token %s\n" line cnum tok;
  end