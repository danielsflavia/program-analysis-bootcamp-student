(* ================================================================
   Exercise 3: Collections and Records -- "Variable Tracker"
   ================================================================ *)

let double_all (_xs : int list) : int list =
  List.map (fun x -> x * 2) _xs

let keep_positive (_xs : int list) : int list =
  List.filter (fun x -> x > 0) _xs

let sum (_xs : int list) : int =
  List.fold_left (fun acc x -> acc + x) 0 _xs

let has_duplicates (xs : string list) : bool =
  let sorted = List.sort String.compare xs in
  let rec check = function
    | [] | [_] -> false
    | a :: b :: rest -> if a = b then true else check (b :: rest)
  in
  check sorted

type assignment = {
  var_name : string;
  value    : int;
  line     : int;
}

let make_assign (_name : string) (_value : int) (_line : int) : assignment =
  { var_name = _name; value = _value; line = _line }

let format_assign (_a : assignment) : string =
  Printf.sprintf "%s = %d (line %d)" _a.var_name _a.value _a.line

let increment_value (_a : assignment) (_n : int) : assignment =
  { _a with value = _a.value + _n }

module StringMap = Map.Make(String)

let build_env (_pairs : (string * int) list) : int StringMap.t =
  List.fold_left (fun acc (k, v) -> StringMap.add k v acc) StringMap.empty _pairs

let lookup_var (_env : int StringMap.t) (_name : string) : int option =
  StringMap.find_opt _name _env

let all_vars (_env : int StringMap.t) : string list =
  List.map fst (StringMap.bindings _env)

module StringSet = Set.Make(String)

let assigned_vars (_assignments : assignment list) : StringSet.t =
  List.fold_left (fun acc a -> StringSet.add a.var_name acc) StringSet.empty _assignments

let common_vars (_s1 : StringSet.t) (_s2 : StringSet.t) : StringSet.t =
  StringSet.inter _s1 _s2

let make_counter () : unit -> int =
  let count = ref 0 in
  fun () ->
    let n = !count in
    count := !count + 1;
    n

let () =
  Printf.printf "=== Exercise 3: Collections and Records ===\n\n";
  let show_ints xs =
    "[" ^ String.concat "; " (List.map string_of_int xs) ^ "]"
  in
  Printf.printf "double_all [1; 2; 3] = %s\n" (show_ints (double_all [1; 2; 3]));
  Printf.printf "keep_positive [-1; 3; 0; 5; -2] = %s\n" (show_ints (keep_positive [-1; 3; 0; 5; -2]));
  Printf.printf "sum [1; 2; 3; 4] = %d\n" (sum [1; 2; 3; 4]);
  Printf.printf "has_duplicates [\"a\"; \"b\"; \"a\"] = %b\n" (has_duplicates ["a"; "b"; "a"]);
  Printf.printf "has_duplicates [\"a\"; \"b\"; \"c\"] = %b\n\n" (has_duplicates ["a"; "b"; "c"]);
  let a1 = make_assign "x" 5 1 in
  let a2 = make_assign "y" 10 2 in
  let a3 = make_assign "x" 7 3 in
  Printf.printf "format_assign a1 = %s\n" (format_assign a1);
  Printf.printf "format_assign a2 = %s\n" (format_assign a2);
  let a1' = increment_value a1 3 in
  Printf.printf "increment_value a1 3 = %s\n\n" (format_assign a1');
  let env = build_env [("x", 1); ("y", 2); ("z", 3)] in
  let print_lookup name =
    match lookup_var env name with
    | Some v -> Printf.printf "lookup_var \"%s\" = Some %d\n" name v
    | None   -> Printf.printf "lookup_var \"%s\" = None\n" name
  in
  print_lookup "x";
  print_lookup "y";
  print_lookup "w";
  Printf.printf "all_vars env = [%s]\n\n" (String.concat "; " (all_vars env));
  let assignments = [a1; a2; a3] in
  let vars = assigned_vars assignments in
  Printf.printf "assigned_vars = {%s}\n" (String.concat ", " (StringSet.elements vars));
  let s1 = StringSet.of_list ["x"; "y"; "z"] in
  let s2 = StringSet.of_list ["y"; "z"; "w"] in
  let common = common_vars s1 s2 in
  Printf.printf "common_vars = {%s}\n\n" (String.concat ", " (StringSet.elements common));
  let next = make_counter () in
  Printf.printf "counter: %d, %d, %d\n" (next ()) (next ()) (next ());
  Printf.printf "\nDone!\n"
