open Shared_ast.Ast_types
open Visitor  (* <- your visitor.ml *)

(** Helper functions to get labels of a statement/expression using label_collector *)

let label_of_stmt s =
  let labels = ref [] in
  let collector =
    object
      inherit label_collector
      method! visit_stmt stmt =
        let lbl =
          match stmt with
          | Assign _ -> "Assign"
          | If _ -> "If"
          | While _ -> "While"
          | Return _ -> "Return"
          | Print _ -> "Print"
          | Block _ -> "Block"
        in
        labels := !labels @ [lbl];
        (* Recurse as usual *)
        match stmt with
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
        | Block stmts -> self#visit_stmt_list stmts
    end
  in
  collector#visit_stmt s;
  !labels

let label_of_expr e =
  let labels = ref [] in
  let collector =
    object
      inherit label_collector
      method! visit_expr expr =
        let lbl =
          match expr with
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
        labels := !labels @ [lbl];
        (* Recurse *)
        match expr with
        | BinOp (_, e1, e2) -> self#visit_expr e1; self#visit_expr e2
        | UnaryOp (_, e1) -> self#visit_expr e1
        | _ -> ()
    end
  in
  collector#visit_expr e;
  !labels

(** Pre-order DFS *)
let pre_order stmts =
  List.flatten (List.map label_of_stmt stmts)

(** Post-order DFS *)
let rec post_order stmts =
  let rec stmt_post s =
    match s with
    | Assign (_, e) -> (post_expr e) @ ["Assign"]
    | If (cond, then_s, else_s) ->
        (post_expr cond) @ (List.flatten (List.map stmt_post then_s))
        @ (List.flatten (List.map stmt_post else_s)) @ ["If"]
    | While (cond, body) ->
        (post_expr cond) @ (List.flatten (List.map stmt_post body)) @ ["While"]
    | Return e -> (post_expr e) @ ["Return"]
    | Print e -> (post_expr e) @ ["Print"]
    | Block ss -> (List.flatten (List.map stmt_post ss)) @ ["Block"]
  and post_expr e =
    match e with
    | IntLit n -> [Printf.sprintf "IntLit(%d)" n]
    | BoolLit b -> [Printf.sprintf "BoolLit(%b)" b]
    | Var x -> [Printf.sprintf "Var(%s)" x]
    | BinOp (op, e1, e2) ->
        (post_expr e1) @ (post_expr e2)
        @ [Printf.sprintf "BinOp(%s)"
            (match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/")]
    | UnaryOp (op, e1) -> (post_expr e1)
                        @ [Printf.sprintf "UnaryOp(%s)"
                            (match op with Neg -> "-" | Not -> "!")]
    | Call f -> [Printf.sprintf "Call(%s)" f]
  in
  List.flatten (List.map stmt_post stmts)

(** BFS using Queue *)
let bfs stmts =
  let open Queue in
  let q = create () in
  List.iter push stmts q;
  let labels = ref [] in
  while not (is_empty q) do
    let s = pop q in
    let rec enqueue_stmt s =
      match s with
      | Assign (_, e) -> labels := !labels @ ["Assign"]; enqueue_expr e
      | If (cond, then_s, else_s) ->
          labels := !labels @ ["If"]; enqueue_expr cond;
          List.iter push then_s q; List.iter push else_s q
      | While (cond, body) ->
          labels := !labels @ ["While"]; enqueue_expr cond;
          List.iter push body q
      | Return e -> labels := !labels @ ["Return"]; enqueue_expr e
      | Print e -> labels := !labels @ ["Print"]; enqueue_expr e
      | Block ss -> labels := !labels @ ["Block"]; List.iter push ss q
    and enqueue_expr e =
      match e with
      | IntLit n -> labels := !labels @ [Printf.sprintf "IntLit(%d)" n]
      | BoolLit b -> labels := !labels @ [Printf.sprintf "BoolLit(%b)" b]
      | Var x -> labels := !labels @ [Printf.sprintf "Var(%s)" x]
      | BinOp (op, e1, e2) ->
          labels := !labels @ [Printf.sprintf "BinOp(%s)"
                                (match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/")];
          push e1 q_expr; push e2 q_expr
      | UnaryOp (op, e1) ->
          labels := !labels @ [Printf.sprintf "UnaryOp(%s)"
                                (match op with Neg -> "-" | Not -> "!")];
          push e1 q_expr
      | Call f -> labels := !labels @ [Printf.sprintf "Call(%s)" f]
    and q_expr = create () in
    enqueue_stmt s;
    while not (is_empty q_expr) do
      let e = pop q_expr in
      enqueue_expr e
    done
  done;
  !labels