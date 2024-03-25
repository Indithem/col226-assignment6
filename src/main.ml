open Secd;;
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
    List.iter (print_ast (p+1)) e
  | Application (e1, e2) ->
    Printf.printf "Application:\n";
    print_ast (p+1) e1;
    print_ast (p+1) e2
  | Ifthenelse (e1, e2, e3) ->
    Printf.printf "If:\n";
    print_ast (p+1) e1;
    print_gaps p;
    Printf.printf "Then:\n";
    print_ast (p+2) e2;
    print_gaps p;
    Printf.printf "Else:\n";
    print_ast (p+2) e3
  | Tuple es ->
    Printf.printf "Tuple List:\n";
    List.iter (fun e -> print_ast (p+1) e) es
  | Project (i, e) ->
    Printf.printf "Projection of {}th element evaluated by \n";
    print_ast (p+1) i;
    print_gaps p;
    Printf.printf "from the tuple:\n";
    print_ast (p+1) e
  | Declaration (v, e) ->
    Printf.printf "Declaration of variable %s\n" v;
    print_ast (p+1) e
  | Function (f, xs, e) ->
    Printf.printf "Function %s with parameters:\n" f;
    List.iter (fun x -> print_gaps (p+1);Printf.printf "%s\n" x) xs;
    print_gaps p;
    Printf.printf "Body:\n";
    List.iter (print_ast (p+1)) e
  | Function_application (f, es) ->
    Printf.printf "Function application of %s with arguments:\n" f;
    List.iter (print_ast (p+1)) es
  | Print e ->
    Printf.printf "Print:\n";
    print_ast (p+1) e
;;

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

let main expr_list =
  Printf.printf "\x1B[33mAst:\n";
  List.iter (print_ast 0) expr_list;
  let ops = List.flatten (List.map compile expr_list)
  in
  Printf.printf "\x1B[34mOp codes:\n";
  print_code ops;
  Printf.printf "\x1B[32m";
  try
    let result = secd_machine [] StringMap.empty ops [] in
    Printf.printf "Result:\n";
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
main expressions
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