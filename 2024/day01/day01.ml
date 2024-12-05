let parse input =
  let open Angstrom in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let p = lift2 (fun a b -> a, b) (integer <* string "   ") integer in
  parse_string ~consume:All p input |> Result.get_ok
;;

let part_one =
  let input = In_channel.input_lines (In_channel.open_text "2024/day01/input") in
  let parsed_list = List.map parse input in
  let l1, l2 = List.split parsed_list in
  let sorted1 = List.fast_sort compare l1 in
  let sorted2 = List.fast_sort compare l2 in
  let combined_list = List.combine sorted1 sorted2 in
  let differences =
    List.map (fun (a, b) -> if a < b then b - a else a - b) combined_list
  in
  List.fold_left (fun acc diff -> acc + diff) 0 differences
in
part_one |> print_int |> print_newline
;;

(* let example = *)
(*   "3   4 *)
     (* 4   3 *)
     (* 2   5 *)
     (* 1   3 *)
     (* 3   9 *)
     (* 3   3" *)

let part_two =
  let module Int_map = Map.Make (Int) in
  let input = In_channel.input_lines (In_channel.open_text "2024/day01/input") in
  (* let input = String.split_on_char '\n' example in *)
  let parsed_list = List.map parse input in
  let l1, l2 = List.split parsed_list in
  let int_map =
    List.fold_left
      (fun imap num ->
        Int_map.update
          num
          (function
            | Some x -> Some (x + 1)
            | None -> Some 1)
          imap)
      Int_map.empty
      l2
  in
  List.fold_left
    (fun acc num ->
      (* print_int acc |> print_newline; *)
      (* print_int num |> print_newline; *)
      match Int_map.find_opt num int_map with
      | Some x -> acc + (x * num)
      | None -> acc)
    0
    l1
in
part_two |> print_int |> print_newline
