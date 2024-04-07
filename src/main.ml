open Krivine;;

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
;;

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
  (* | Function (f, xs, e) ->
    Printf.printf "Function %s with parameters:\n" f;
    List.iter (fun x -> print_gaps (p+1);Printf.printf "%s\n" x) xs;
    print_gaps p;
    Printf.printf "Body:\n";
    List.iter (print_ast (p+1)) e
  | Function_application (f, es) ->
    Printf.printf "Function application of %s with arguments:\n" f;
    List.iter (print_ast (p+1)) es *)
  (* | Print e ->
    Printf.printf "Print:\n";
    print_ast (p+1) e *)
;;

let print_krivine_expression p e =
  print_ast p e

let rec print_closure p c =
  match c with
  | CLOSURE (e, env) ->
      print_gaps p; Printf.printf "Closure with expression:\n";
      print_krivine_expression (p+1) e;
      print_gaps p; Printf.printf "Environment:\n";
      StringMap.iter (fun k v -> print_gaps (p+1);Printf.printf "Variable: %s\n" k;print_closure (p+2) v) env
;;
let main expr_list =
  let p = ref 0 in
  let evaluate env krivine_expression = 
    Printf.printf "\x1B[00mEvaluating expression %d\n" !p;
    if Sys.getenv "DEBUG" = "1" then (
      print_closure 0 (CLOSURE(krivine_expression, env));
    );
    p := !p + 1;
    try
      let result = 
      krivine_machine (CLOSURE(krivine_expression, env)) [] 
      in
      Printf.printf "\x1B[32mResult:\n";
      print_closure 0 result;
      match result with CLOSURE (_, env) -> env
    with
      | Krivine_error(error, stack) -> Printf.printf "\x1B[31mError: %s\n" error; List.iter (print_closure 0) stack; env
  in
  if Sys.getenv "DEBUG" = "1" then (
    Printf.printf "\x1B[33mAst:\n";
    List.iter (print_ast 0) expr_list;
  );
  let krivine_expressions = (List.map compile expr_list) in
  if Sys.getenv "DEBUG" = "1" then (
    Printf.printf "\x1B[34mCompiled:\n";
    List.iter (print_ast 0) krivine_expressions;
  );
  List.fold_left evaluate StringMap.empty krivine_expressions;
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