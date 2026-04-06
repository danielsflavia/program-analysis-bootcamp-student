(** Generic iterative dataflow analysis solver. *)

type direction = Forward | Backward

type 'a analysis = {
  direction : direction;
  init : 'a;
  merge : 'a -> 'a -> 'a;
  transfer : string -> 'a -> 'a;
  equal : 'a -> 'a -> bool;
}

module StringMap = Map.Make (String)

let solve (analysis : 'a analysis)
    (cfg : (string * string list * string list) list)
    : (string * 'a * 'a) list =
  (* Initialize IN and OUT for every block to analysis.init *)
  let init_map =
    List.fold_left (fun m (label, _, _) ->
      StringMap.add label analysis.init m
    ) StringMap.empty cfg
  in
  let in_map  = ref init_map in
  let out_map = ref init_map in

  (* Helper: merge a list of values using analysis.merge *)
  let merge_all vals =
    List.fold_left analysis.merge analysis.init vals
  in

  (* Iterate until fixpoint *)
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (label, preds, succs) ->
      match analysis.direction with
      | Forward ->
        (* IN[B] = merge over all predecessors OUT[P] *)
        let pred_vals = List.map (fun p -> StringMap.find p !out_map) preds in
        let new_in = merge_all pred_vals in
        (* OUT[B] = transfer(B, IN[B]) *)
        let new_out = analysis.transfer label new_in in
        if not (analysis.equal new_in  (StringMap.find label !in_map)) ||
           not (analysis.equal new_out (StringMap.find label !out_map)) then begin
          changed := true;
          in_map  := StringMap.add label new_in  !in_map;
          out_map := StringMap.add label new_out !out_map
        end
      | Backward ->
        (* OUT[B] = merge over all successors IN[S] *)
        let succ_vals = List.map (fun s -> StringMap.find s !in_map) succs in
        let new_out = merge_all succ_vals in
        (* IN[B] = transfer(B, OUT[B]) *)
        let new_in = analysis.transfer label new_out in
        if not (analysis.equal new_out (StringMap.find label !out_map)) ||
           not (analysis.equal new_in  (StringMap.find label !in_map)) then begin
          changed := true;
          out_map := StringMap.add label new_out !out_map;
          in_map  := StringMap.add label new_in  !in_map
        end
    ) cfg
  done;

  (* Build result list *)
  List.map (fun (label, _, _) ->
    (label,
     StringMap.find label !in_map,
     StringMap.find label !out_map)
  ) cfg
