(* ================================================================
   Exercise 4: Modules and Functors -- "Analysis Domain Builder"
   ================================================================ *)

module type LATTICE = sig
  type t
  val bottom : t
  val top : t
  val join : t -> t -> t
  val equal : t -> t -> bool
  val to_string : t -> string
end

module BoolLattice : LATTICE with type t = bool = struct
  type t = bool
  let bottom = false
  let top = true
  let join a b = a || b
  let equal a b = (a = b)
  let to_string b = if b then "true" else "false"
end

module BoolPrint = struct
  let to_string (b : bool) : string =
    if b then "T" else "F"
end

type three_value = Bot | Zero | Positive | Unknown

module ThreeValueLattice : LATTICE with type t = three_value = struct
  type t = three_value
  let bottom = Bot
  let top = Unknown

  let join (a : t) (b : t) : t =
    match a, b with
    | Bot, x | x, Bot -> x
    | x, y -> if x = y then x else Unknown

  let equal (a : t) (b : t) : bool =
    a = b

  let to_string (v : t) : string =
    match v with
    | Bot -> "Bot"
    | Zero -> "Zero"
    | Positive -> "Positive"
    | Unknown -> "Unknown"
end

module MakeEnv (L : LATTICE) = struct
  module M = Map.Make(String)
  type t = L.t M.t
  let empty : t = M.empty

  let lookup (env : t) (x : string) : L.t =
    match M.find_opt x env with
    | Some v -> v
    | None -> L.bottom

  let update (env : t) (x : string) (v : L.t) : t =
    M.add x v env

  let join (env1 : t) (env2 : t) : t =
    M.union (fun _key v1 v2 -> Some (L.join v1 v2)) env1 env2

  let to_string (env : t) : string =
    let pairs = M.bindings env in
    let entries =
      List.map (fun (k, v) -> k ^ " -> " ^ L.to_string v) pairs
    in
    "{" ^ String.concat ", " entries ^ "}"
end

module Env = MakeEnv(ThreeValueLattice)

let () =
  Printf.printf "=== Exercise 4: Modules and Functors ===\n\n";
  Printf.printf "-- BoolLattice --\n";
  Printf.printf "BoolPrint.to_string true = %s\n" (BoolPrint.to_string true);
  Printf.printf "BoolPrint.to_string false = %s\n" (BoolPrint.to_string false);
  Printf.printf "BoolLattice.join false true = %s\n"
    (BoolLattice.to_string (BoolLattice.join false true));
  Printf.printf "BoolLattice.equal true true = %b\n\n"
    (BoolLattice.equal true true);
  Printf.printf "-- ThreeValueLattice --\n";
  Printf.printf "bottom = %s\n" (ThreeValueLattice.to_string ThreeValueLattice.bottom);
  Printf.printf "top = %s\n" (ThreeValueLattice.to_string ThreeValueLattice.top);
  Printf.printf "join Bot Zero = %s\n"
    (ThreeValueLattice.to_string (ThreeValueLattice.join Bot Zero));
  Printf.printf "join Zero Positive = %s\n"
    (ThreeValueLattice.to_string (ThreeValueLattice.join Zero Positive));
  Printf.printf "join Positive Positive = %s\n"
    (ThreeValueLattice.to_string (ThreeValueLattice.join Positive Positive));
  Printf.printf "equal Zero Zero = %b\n"
    (ThreeValueLattice.equal Zero Zero);
  Printf.printf "equal Zero Positive = %b\n\n"
    (ThreeValueLattice.equal Zero Positive);
  Printf.printf "-- MakeEnv(ThreeValueLattice) --\n";
  let env0 = Env.empty in
  Printf.printf "empty = %s\n" (Env.to_string env0);
  Printf.printf "lookup empty \"x\" = %s\n"
    (ThreeValueLattice.to_string (Env.lookup env0 "x"));
  let env1 = Env.update env0 "x" Zero in
  let env1 = Env.update env1 "y" Positive in
  Printf.printf "env1 = %s\n" (Env.to_string env1);
  let env2 = Env.update Env.empty "x" Positive in
  let env2 = Env.update env2 "z" Zero in
  Printf.printf "env2 = %s\n" (Env.to_string env2);
  let merged = Env.join env1 env2 in
  Printf.printf "join env1 env2 = %s\n" (Env.to_string merged);
  Printf.printf "\nDone!\n"
