(* ================================================================
   Exercise 2: Types and Recursion -- "Mini Expression Tree"
   ================================================================ *)

type op =
  | Add
  | Sub
  | Mul

type expr =
  | Num of int
  | Var of string
  | BinOp of op * expr * expr

let string_of_op (_o : op) : string =
  match _o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
[@@warning "-32"]

let rec string_of_expr (_e : expr) : string =
  match _e with
  | Num n -> string_of_int n
  | Var x -> x
  | BinOp (o, l, r) ->
    "(" ^ string_of_expr l ^ " " ^ string_of_op o ^ " " ^ string_of_expr r ^ ")"

let rec count_nodes (_e : expr) : int =
  match _e with
  | Num _ -> 1
  | Var _ -> 1
  | BinOp (_, l, r) -> 1 + count_nodes l + count_nodes r

let rec depth (_e : expr) : int =
  match _e with
  | Num _ -> 1
  | Var _ -> 1
  | BinOp (_, l, r) -> 1 + max (depth l) (depth r)

let rec eval (_e : expr) : int option =
  match _e with
  | Num n -> Some n
  | Var _ -> None
  | BinOp (o, l, r) ->
    match eval l, eval r with
    | Some lv, Some rv ->
      (match o with
       | Add -> Some (lv + rv)
       | Sub -> Some (lv - rv)
       | Mul -> Some (lv * rv))
    | _ -> None

let rec substitute (_var_name : string) (_value : int) (_e : expr) : expr =
  match _e with
  | Num n -> Num n
  | Var x -> if x = _var_name then Num _value else Var x
  | BinOp (o, l, r) ->
    BinOp (o, substitute _var_name _value l, substitute _var_name _value r)

let vars_in (_e : expr) : string list =
  let rec collect (_e : expr) : string list =
    match _e with
    | Num _ -> []
    | Var x -> [x]
    | BinOp (_, l, r) -> collect l @ collect r
  in
  List.sort_uniq String.compare (collect _e)

let is_constant (_e : expr) : bool =
  vars_in _e = []

let rec simplify (_e : expr) : expr =
  match _e with
  | Num n -> Num n
  | Var x -> Var x
  | BinOp (o, l, r) ->
    let l' = simplify l in
    let r' = simplify r in
    match l', r' with
    | Num lv, Num rv ->
      (match o with
       | Add -> Num (lv + rv)
       | Sub -> Num (lv - rv)
       | Mul -> Num (lv * rv))
    | _ -> BinOp (o, l', r')

let () =
  Printf.printf "=== Exercise 2: Types and Recursion ===\n\n";
  let e1 = BinOp (Add, Num 2, Num 3) in
  let e2 = BinOp (Mul, Var "x", BinOp (Add, Num 1, Var "y")) in
  let e3 = BinOp (Sub, BinOp (Add, Num 10, Num 20), Num 5) in
  Printf.printf "string_of_expr e1 = %s\n" (string_of_expr e1);
  Printf.printf "string_of_expr e2 = %s\n" (string_of_expr e2);
  Printf.printf "string_of_expr e3 = %s\n\n" (string_of_expr e3);
  Printf.printf "count_nodes e1 = %d\n" (count_nodes e1);
  Printf.printf "count_nodes e2 = %d\n" (count_nodes e2);
  Printf.printf "depth e1 = %d\n" (depth e1);
  Printf.printf "depth e2 = %d\n\n" (depth e2);
  let print_eval label e =
    match eval e with
    | Some n -> Printf.printf "%s = Some %d\n" label n
    | None   -> Printf.printf "%s = None\n" label
  in
  print_eval "eval e1" e1;
  print_eval "eval e2" e2;
  print_eval "eval e3" e3;
  Printf.printf "\n";
  let e2_sub = substitute "x" 3 e2 in
  Printf.printf "substitute \"x\" 3 e2 = %s\n" (string_of_expr e2_sub);
  print_eval "eval (substitute \"x\" 3 e2)" e2_sub;
  Printf.printf "vars_in e2 = [%s]\n" (String.concat "; " (vars_in e2));
  Printf.printf "is_constant e1 = %b\n" (is_constant e1);
  Printf.printf "is_constant e2 = %b\n" (is_constant e2);
  Printf.printf "simplify e3 = %s\n" (string_of_expr (simplify e3));
  Printf.printf "simplify e2 = %s\n" (string_of_expr (simplify e2));
  Printf.printf "\nDone!\n"
