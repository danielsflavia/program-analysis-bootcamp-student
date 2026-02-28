(* ================================================================
   Exercise 5: Calculator Parser -- Main Driver
   ================================================================ *)

open Ast

let string_of_op (_o : op) : string =
  match _o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
[@@warning "-32"]

let rec string_of_expr (_e : expr) : string =
  match _e with
  | Num n -> string_of_int n
  | Var x -> x
  | Neg e -> "(- " ^ string_of_expr e ^ ")"
  | BinOp (o, l, r) ->
    "(" ^ string_of_expr l ^ " " ^ string_of_op o ^ " " ^ string_of_expr r ^ ")"

let rec eval (_e : expr) : int option =
  match _e with
  | Num n -> Some n
  | Var _ -> None
  | Neg e ->
    (match eval e with
     | Some n -> Some (-n)
     | None -> None)
  | BinOp (o, l, r) ->
    match eval l, eval r with
    | Some lv, Some rv ->
      (match o with
       | Add -> Some (lv + rv)
       | Sub -> Some (lv - rv)
       | Mul -> Some (lv * rv)
       | Div -> if rv = 0 then None else Some (lv / rv))
    | _ -> None

let parse_string (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  Parser.program Lexer.token lexbuf

let test_expr (s : string) : unit =
  Printf.printf "Input:  %s\n" s;
  let e = parse_string s in
  Printf.printf "AST:    %s\n" (string_of_expr e);
  (match eval e with
   | Some n -> Printf.printf "Result: %d\n" n
   | None   -> Printf.printf "Result: <cannot evaluate>\n");
  Printf.printf "\n"

let () =
  Printf.printf "=== Exercise 5: Calculator Parser ===\n\n";
  test_expr "42";
  test_expr "1 + 2";
  test_expr "3 * 4 + 5";
  test_expr "(3 + 4) * 5";
  test_expr "10 - 3 - 2";
  test_expr "-7";
  test_expr "x + 1";
  Printf.printf "Done!\n"
