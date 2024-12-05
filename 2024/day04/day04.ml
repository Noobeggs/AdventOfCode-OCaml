(* let example = *)
(*   "MMMSXXMASM\n\ *)
     (*    MSAMXMSMSA\n\ *)
     (*    AMXSXMAAMM\n\ *)
     (*    MSAMASMSMX\n\ *)
     (*    XMASAMXAMM\n\ *)
     (*    XXAMMXXAMA\n\ *)
     (*    SMSMSASXSS\n\ *)
     (*    SAXAMASAAA\n\ *)
     (*    MAMMMXMMMM\n\ *)
     (*    MXMXAXMASX" *)
(* ;; *)

let find word grid =
  let chars = List.of_seq (String.to_seq word) in
  let directions = [ -1, -1; 0, -1; 1, -1; -1, 0; 1, 0; -1, 1; 0, 1; 1, 1 ] in
  let is_char_at char coord =
    match Util.CMap.find_opt coord grid with
    | Some x -> x = char
    | None -> false
  in
  let rec search coord dir = function
    | [] -> true
    | hd :: tl ->
      if is_char_at hd coord
      then search (Util.Coordinate.( + ) coord dir) dir tl
      else false
  in
  Util.CMap.fold
    (fun coord _ sum ->
      sum
      + List.fold_left
          (fun found dir -> if search coord dir chars then found + 1 else found)
          0
          directions)
    grid
    0
;;

let () =
  let input = Util.get_input_lines "2024/day04/input" in
  (* let input = String.split_on_char '\n' example in *)
  let char_map = Util.CMap.empty in
  let rows, char_map =
    List.fold_left
      (fun (row, map) _row ->
        let chars = List.of_seq (String.to_seq _row) in
        let _, cmap =
          List.fold_left
            (fun (col, map) char -> col + 1, Util.CMap.add (row, col) char map)
            (0, map)
            chars
        in
        row + 1, cmap)
      (0, char_map)
      input
  in
  let () =
    Util.CMap.cardinal char_map |> print_int |> print_newline;
    print_string "Rows: ";
    print_int rows |> print_newline
  in
  find "XMAS" char_map |> print_int |> print_newline |> print_newline
;;

(* Horrendous, absolutely horrendous. *)
let find_xword word grid =
  let chars = List.of_seq (String.to_seq word) in
  let midpoint = List.length chars / 2 in
  let midchar = List.nth chars midpoint in
  let rec prefix k = function
    | [] -> []
    | c :: chars -> if k > 0 then c :: prefix (k - 1) chars else []
  in
  let postfix k chars = prefix k (List.rev chars) in
  let prefix_chars = prefix midpoint chars in
  let postfix_chars = postfix midpoint chars in
  let () =
    print_string (String.concat " " (List.map (String.make 1) prefix_chars))
    |> print_newline;
    print_string (String.concat " " (List.map (String.make 1) postfix_chars))
    |> print_newline
  in
  let dir_tlbr = [ (-1, -1), (1, 1); (1, 1), (-1, -1) ] in
  let dir_bltr = [ (-1, 1), (1, -1); (1, -1), (-1, 1) ] in
  let is_char_at char coord =
    match Util.CMap.find_opt coord grid with
    | Some x -> x = char
    | None -> false
  in
  let rec search coord dir = function
    | [] -> true
    | hd :: tl ->
      if is_char_at hd coord
      then search (Util.Coordinate.( + ) coord dir) dir tl
      else false
  in
  let is_xword coord diag_a diag_b =
    List.fold_left
      (fun acc (dir_a, dir_b) ->
        acc
        || (search (Util.Coordinate.( + ) coord dir_a) dir_a prefix_chars
            && search (Util.Coordinate.( + ) coord dir_b) dir_b postfix_chars))
      false
      diag_a
    && List.fold_left
         (fun acc (dir_a, dir_b) ->
           acc
           || (search (Util.Coordinate.( + ) coord dir_a) dir_a prefix_chars
               && search (Util.Coordinate.( + ) coord dir_b) dir_b postfix_chars))
         false
         diag_b
  in
  Util.CMap.fold
    (fun coord char count ->
      if char <> midchar
      then count
      else if is_xword coord dir_tlbr dir_bltr
      then (* let () = Util.Coordinate.pp Format.std_formatter coord in *)
        count + 1
      else count)
    grid
    0
;;

let () =
  let input = Util.get_input_lines "2024/day04/input" in
  (* let input = String.split_on_char '\n' example in *)
  let char_map = Util.CMap.empty in
  let _, char_map =
    List.fold_left
      (fun (row, map) _row ->
        let chars = List.of_seq (String.to_seq _row) in
        let _, cmap =
          List.fold_left
            (fun (col, map) char -> col + 1, Util.CMap.add (row, col) char map)
            (0, map)
            chars
        in
        row + 1, cmap)
      (0, char_map)
      input
  in
  find_xword "MAS" char_map |> print_int |> print_newline
;;
