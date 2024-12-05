let parse input =
  let open Angstrom in
  let integer = take_while1 Util.P.is_digit >>| int_of_string in
  let edges =
    sep_by1 (char '\n') (lift2 (fun a b -> a, b) (integer <* char '|') integer)
  in
  let updates = sep_by1 (char '\n') (sep_by1 (char ',') integer) in
  let p = lift2 (fun e u -> e, u) (edges <* string "\n\n") updates in
  parse_string ~consume:All p input |> Result.get_ok
;;

let rec is_correct edges update =
  let check page remaining =
    List.fold_left
      (fun acc remaining_hd ->
        if List.exists (fun (a, b) -> b = page && a = remaining_hd) edges
        then acc && false
        else acc)
      true
      remaining
  in
  match update with
  | [] -> true
  | hd :: tl -> if check hd tl then is_correct edges tl else false
;;

let part_one =
  let input = Util.get_input "2024/day05/input" in
  let edges, updates = parse input in
  let correct_updates = List.filter (fun update -> is_correct edges update) updates in
  List.fold_left
    (fun acc update -> acc + List.nth update (List.length update / 2))
    0
    correct_updates
in
part_one |> print_int |> print_newline

let topo_sort edges update =
  let rec dfs acc node =
    if List.mem node acc
    then acc
    else (
      let next_nodes = List.filter (fun (a, b) -> a = node && List.mem b update) edges in
      let acc = List.fold_left (fun acc (_, b) -> dfs acc b) acc next_nodes in
      node :: acc)
  in
  List.fold_left
    (fun acc node -> if not (List.mem node acc) then dfs acc node else acc)
    []
    update
;;

(* let example = *)
(*   "47|53\n\ *)
     (*    97|13\n\ *)
     (*    97|61\n\ *)
     (*    97|47\n\ *)
     (*    75|29\n\ *)
     (*    61|13\n\ *)
     (*    75|53\n\ *)
     (*    29|13\n\ *)
     (*    97|29\n\ *)
     (*    53|29\n\ *)
     (*    61|53\n\ *)
     (*    97|53\n\ *)
     (*    61|29\n\ *)
     (*    47|13\n\ *)
     (*    75|47\n\ *)
     (*    97|75\n\ *)
     (*    47|61\n\ *)
     (*    75|61\n\ *)
     (*    47|29\n\ *)
     (*    75|13\n\ *)
     (*    53|13\n\n\ *)
     (*    75,47,61,53,29\n\ *)
     (*    97,61,53,29,13\n\ *)
     (*    75,29,13\n\ *)
     (*    75,97,47,61,53\n\ *)
     (*    61,13,29\n\ *)
     (*    97,13,75,29,47" *)
(* ;; *)

let part_two =
  let input = Util.get_input "2024/day05/input" in
  (* let input = example in *)
  let edges, updates = parse input in
  let wrong_updates = List.filter (fun update -> not (is_correct edges update)) updates in
  (* let _ = *)
  (*   List.map *)
  (*     (fun list -> *)
  (*       print_string (String.concat " " (List.map string_of_int list)) |> print_newline; *)
  (*       print_string (String.concat " " (List.map string_of_int (topo_sort edges list))) *)
  (*       |> print_newline) *)
  (*     wrong_updates *)
  (* in *)
  List.fold_left
    (fun acc update -> acc + List.nth (topo_sort edges update) (List.length update / 2))
    0
    wrong_updates
in
part_two |> print_int |> print_newline
