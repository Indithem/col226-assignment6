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


let lexbufr = Lexing.from_channel stdin in
try
let stmts = Parser.main Lexer.lexer lexbufr in
Printf.printf "Parsed %d statements\n" (List.length stmts);
List.iter (print_ast 0) stmts;
with exn ->
  begin
    let curr = lexbufr.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbufr in
    (* let tail = Sql_lexer.ruleTail "" lexbufr in *)
    Printf.printf "Syntax error at line %d, character %d, token %s\n" line cnum tok;
  end