(* visitor.ml - Visitor pattern for AST traversal *)

open Shared_ast.Ast_types

(* Abstract visitor class *)
class virtual ['a] visitor =
  object (self)
    method virtual visit_stmt : stmt -> 'a
    method virtual visit_expr : expr -> 'a

    method visit_stmt_list stmts =
      List.iter (fun s -> ignore (self#visit_stmt s)) stmts

    method visit_expr_list exprs =
      List.iter (fun e -> ignore (self#visit_expr e)) exprs
  end

(* Concrete visitor that collects labels *)
class label_collector =
  object
    inherit [unit] visitor

    (* Visit a statement *)
    method visit_stmt stmt =
      let label =
        match stmt with
        | Assign _ -> "Assign"
        | If _ -> "If"
        | While _ -> "While"
        | Return _ -> "Return"
        | Print _ -> "Print"
        | Block _ -> "Block"
      in
      print_endline label;
      (* Recurse into children *)
      (match stmt with
      | Assign (_, e) -> self#visit_expr e
      | If (cond, then_s, else_s) ->
          self#visit_expr cond;
          self#visit_stmt_list then_s;
          self#visit_stmt_list else_s
      | While (cond, body) ->
          self#visit_expr cond;
          self#visit_stmt_list body
      | Return e -> self#visit_expr e
      | Print e -> self#visit_expr e
      | Block stmts -> self#visit_stmt_list stmts)

    (* Visit an expression *)
    method visit_expr e =
      let label =
        match e with
        | IntLit n -> Printf.sprintf "IntLit(%d)" n
        | BoolLit b -> Printf.sprintf "BoolLit(%b)" b
        | Var x -> Printf.sprintf "Var(%s)" x
        | BinOp (op, _, _) ->
            let op_str = match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" in
            Printf.sprintf "BinOp(%s)" op_str
        | UnaryOp (op, _) ->
            let op_str = match op with Neg -> "-" | Not -> "!" in
            Printf.sprintf "UnaryOp(%s)" op_str
        | Call f -> Printf.sprintf "Call(%s)" f
      in
      print_endline label;
      (* Recurse into children *)
      match e with
      | BinOp (_, e1, e2) -> self#visit_expr e1; self#visit_expr e2
      | UnaryOp (_, e1) -> self#visit_expr e1
      | _ -> ()
  end