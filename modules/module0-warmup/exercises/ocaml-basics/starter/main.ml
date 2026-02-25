(* ================================================================
   Exercise 1: OCaml Basics -- "Token Classifier"
   ================================================================ *)

let square (_x : int) : int =
  _x * _x

let is_empty (_s : string) : bool =
  String.length _s = 0

let greet (_name : string) : string =
  "Hello, " ^ _name ^ "!"

let is_digit (_c : char) : bool =
  '0' <= _c && _c <= '9'
[@@warning "-32"]

let is_alpha (_c : char) : bool =
  ('a' <= _c && _c <= 'z') || ('A' <= _c && _c <= 'Z') || _c = '_'
[@@warning "-32"]

let classify_char (_c : char) : string =
  if is_digit _c then "digit"
  else if is_alpha _c then "alpha"
  else if _c = '+' || _c = '-' || _c = '*' || _c = '/' then "operator"
  else "unknown"

type token = string * string

let format_token ((_cat, _text) : token) : string =
  "[" ^ _cat ^ ": " ^ _text ^ "]"

let make_token (_text : string) : token =
  if String.length _text = 0 then ("empty", "")
  else if is_digit _text.[0] then ("number", _text)
  else if is_alpha _text.[0] then ("identifier", _text)
  else ("symbol", _text)

type pos = int * int

let format_pos ((_line, _col) : pos) : string =
  Printf.sprintf "line %d, col %d" _line _col

let advance_pos ((_line, _col) : pos) (_c : char) : pos =
  if _c = '\n' then (_line + 1, 1)
  else (_line, _col + 1)

let scan_positions (s : string) : pos =
  let len = String.length s in
  let rec go i ((line, col) : pos) : pos =
    if i >= len then (line, col)
    else go (i + 1) (advance_pos (line, col) s.[i])
  in
  go 0 (1, 1)

let () =
  Printf.printf "=== Exercise 1: OCaml Basics ===\n\n";
  Printf.printf "square 5 = %d\n" (square 5);
  Printf.printf "square (-3) = %d\n" (square (-3));
  Printf.printf "is_empty \"\" = %b\n" (is_empty "");
  Printf.printf "is_empty \"hi\" = %b\n" (is_empty "hi");
  Printf.printf "greet \"OCaml\" = %s\n\n" (greet "OCaml");
  Printf.printf "classify_char '7' = %s\n" (classify_char '7');
  Printf.printf "classify_char 'x' = %s\n" (classify_char 'x');
  Printf.printf "classify_char '+' = %s\n" (classify_char '+');
  Printf.printf "classify_char '!' = %s\n\n" (classify_char '!');
  Printf.printf "format_token (\"keyword\", \"if\") = %s\n" (format_token ("keyword", "if"));
  Printf.printf "make_token \"42\" = %s\n" (format_token (make_token "42"));
  Printf.printf "make_token \"hello\" = %s\n" (format_token (make_token "hello"));
  Printf.printf "make_token \"+\" = %s\n" (format_token (make_token "+"));
  Printf.printf "make_token \"\" = %s\n\n" (format_token (make_token ""));
  Printf.printf "format_pos (1, 1) = %s\n" (format_pos (1, 1));
  Printf.printf "advance_pos (1,1) 'a' = %s\n" (format_pos (advance_pos (1, 1) 'a'));
  Printf.printf "advance_pos (1,3) '\\n' = %s\n" (format_pos (advance_pos (1, 3) '\n'));
  Printf.printf "scan_positions \"ab\\ncd\" = %s\n" (format_pos (scan_positions "ab\ncd"));
  Printf.printf "\nDone!\n"
