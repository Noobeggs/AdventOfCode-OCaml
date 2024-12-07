let example =
  "190: 10 19\n\
   3267: 81 40 27\n\
   83: 17 5\n\
   156: 15 6\n\
   7290: 6 8 6 15\n\
   161011: 16 10 13\n\
   192: 17 8 14\n\
   21037: 9 7 18 13\n\
   292: 11 6 16 20"
;;

let parse input =
  let open Angstrom in
  let integer = take_while1 Util.P.is_digit >>| int_of_string in
  let equation =
    lift2 (fun a b -> a, b) (integer <* string ": ") (sep_by1 (char ' ') integer)
  in
  let equations = sep_by1 (char '\n') equation in
  parse_string ~consume:All equations input |> Result.get_ok
;;

let rec permutate acc target lst =
  (* print_int acc; *)
  (* print_string " "; *)
  (* print_int target |> print_newline; *)
  if acc > target
  then false
  else if acc = target
  then true
  else (
    match lst with
    | x :: y :: tl when acc = 0 ->
      permutate (x + y) target tl || permutate (x * y) target tl
    | x :: tl -> permutate (acc + x) target tl || permutate (acc * x) target tl
    | _ -> false)
;;

let part_one_example =
  let input = example in
  let equations = parse input in
  (* let () = print_int (List.length equations) |> print_newline in *)
  List.fold_left
    (fun sum (test, numbers) -> if permutate 0 test numbers then sum + test else sum)
    0
    equations
in
part_one_example |> print_int |> print_newline
;;

let part_one =
  let input = Util.get_input "2024/day07/input" in
  let equations = parse input in
  (* let () = print_int (List.length equations) |> print_newline in *)
  List.fold_left
    (fun sum (test, numbers) -> if permutate 0 test numbers then sum + test else sum)
    0
    equations
in
part_one |> print_int |> print_newline

let concat x y =
  let rec length acc n = if n / 10 > 0 then length (acc + 1) (n / 10) else acc in
  let rec pow10 acc n = if n = 1 then acc else pow10 (acc * 10) (n - 1) in
  (x * pow10 10 (length 1 y)) + y
;;

let rec permutate2 acc target lst =
  if acc > target
  then false
  else if acc = target
  then true
  else (
    match lst with
    | x :: y :: tl when acc = 0 ->
      permutate2 (x + y) target tl
      || permutate2 (x * y) target tl
      || permutate2 (concat x y) target tl
    | x :: tl ->
      permutate2 (acc + x) target tl
      || permutate2 (acc * x) target tl
      || permutate2 (concat acc x) target tl
    | _ -> false)
;;

let part_two_example =
  let input = example in
  let equations = parse input in
  (* let () = print_int (List.length equations) |> print_newline in *)
  List.fold_left
    (fun sum (test, numbers) -> if permutate2 0 test numbers then sum + test else sum)
    0
    equations
in
part_two_example |> print_int |> print_newline
;;

let part_two =
  let input = Util.get_input "2024/day07/input" in
  let equations = parse input in
  (* let () = print_int (List.length equations) |> print_newline in *)
  List.fold_left
    (fun sum (test, numbers) -> if permutate2 0 test numbers then sum + test else sum)
    0
    equations
in
part_two |> print_int |> print_newline
