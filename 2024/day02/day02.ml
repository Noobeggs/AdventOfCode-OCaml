let parse input =
  let open Angstrom in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let report = sep_by1 (char ' ') integer in
  let reports = sep_by1 (char '\n') report in
  parse_string ~consume:All reports input |> Result.get_ok
;;

let is_safe report =
  let rec is_increasing = function
    | x :: y :: tl -> if x < y && y - x <= 3 then is_increasing (y :: tl) else false
    | _ -> true
  in
  let rec is_decreasing = function
    | x :: y :: tl -> if x > y && x - y <= 3 then is_decreasing (y :: tl) else false
    | _ -> true
  in
  is_increasing report || is_decreasing report
;;

let () =
  let input = In_channel.input_all (In_channel.open_text "2024/day02/input") in
  let reports = parse input in
  List.fold_left (fun acc report -> if is_safe report then acc + 1 else acc) 0 reports
  |> print_int
  |> print_newline
;;

let is_safe2 report =
  let rec aux acc lst prefix =
    match lst with
      | x :: tl -> aux (List.rev_append prefix tl :: acc) tl (x :: prefix)
      | [] -> acc
  in
  let sublists = aux [] report [] in
  is_safe report || List.fold_left (fun acc lst -> acc || is_safe lst) false sublists
;;

(*   let example = *)
(*   "7 6 4 2 1 *)
(* 1 2 7 8 9 *)
(* 9 7 6 2 1 *)
(* 1 3 2 4 5 *)
(* 8 6 4 4 1 *)
(* 1 3 6 7 9" *)

let () =
  let input = In_channel.input_all (In_channel.open_text "2024/day02/input") in
  let reports = parse input in
  (* let reports = parse example in *)
  List.fold_left (fun acc report -> if is_safe2 report then acc + 1 else acc) 0 reports
  |> print_int
  |> print_newline
;;
