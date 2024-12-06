module Guard = struct
  type t = Util.Coordinate.t * [ `GuardUp | `GuardDown | `GuardLeft | `GuardRight ]

  let compare = compare
end

module GuardSet = Set.Make (Guard)

let parse_line line =
  let open Angstrom in
  let nothing = char '.' *> return `Nothing in
  let obstacle = char '#' *> return `Obstacle in
  let guard_up = char '^' *> return `GuardUp in
  let guard_down = char 'v' *> return `GuardDown in
  let guard_left = char '<' *> return `GuardLeft in
  let guard_right = char '>' *> return `GuardRight in
  parse_string
    ~consume:All
    (obstacle
     <|> nothing
     <|> guard_down
     <|> guard_left
     <|> guard_right
     <|> guard_up
     |> many)
    line
  |> Result.get_ok
;;

let parse input =
  let tokens = List.map parse_line input in
  let obstacle_map = Util.CMap.empty in
  let guard = (-1, -1), `GuardUp in
  let _row, (guard_coord, guard_dir), obstacle_map =
    List.fold_left
      (fun (row_num, (coord, dir), map) row ->
        let _col, (coord, dir), map =
          List.fold_left
            (fun (col_num, guard, map) -> function
              | `Nothing -> col_num + 1, guard, map
              | `Obstacle ->
                col_num + 1, guard, Util.CMap.add (row_num, col_num) `Obstacle map
              | (`GuardUp | `GuardDown | `GuardLeft | `GuardRight) as guard_type ->
                col_num + 1, ((row_num, col_num), guard_type), map)
            (0, (coord, dir), map)
            row
        in
        row_num + 1, (coord, dir), map)
      (0, guard, obstacle_map)
      tokens
  in
  (guard_coord, guard_dir), obstacle_map
;;

let guard_movement coord dir map =
  let has_obstacle coord =
    match Util.CMap.find_opt coord map with
    | Some _ -> true
    | None -> false
  in
  match dir with
  | `GuardUp ->
    if has_obstacle (Util.Coordinate.( + ) coord (-1, 0))
    then coord, `GuardRight
    else Util.Coordinate.( + ) coord (-1, 0), `GuardUp
  | `GuardDown ->
    if has_obstacle (Util.Coordinate.( + ) coord (1, 0))
    then coord, `GuardLeft
    else Util.Coordinate.( + ) coord (1, 0), `GuardDown
  | `GuardLeft ->
    if has_obstacle (Util.Coordinate.( + ) coord (0, -1))
    then coord, `GuardUp
    else Util.Coordinate.( + ) coord (0, -1), `GuardLeft
  | `GuardRight ->
    if has_obstacle (Util.Coordinate.( + ) coord (0, 1))
    then coord, `GuardDown
    else Util.Coordinate.( + ) coord (0, 1), `GuardRight
;;

let rec count_steps acc (coord, dir) map rows cols =
  let x, y = coord in
  let condition = x < rows && x >= 0 && y < cols && y >= 0 in
  (* let () = Util.Coordinate.pp Format.std_formatter coord in *)
  if condition
  then count_steps (Util.CSet.add coord acc) (guard_movement coord dir map) map rows cols
  else acc
;;

let example =
  "....#.....\n\
   .........#\n\
   ..........\n\
   ..#.......\n\
   .......#..\n\
   ..........\n\
   .#..^.....\n\
   ........#.\n\
   #.........\n\
   ......#..."
;;

let part_one_example =
  let input = String.split_on_char '\n' example in
  let (guard_coord, guard_dir), obstacle_map = parse input in
  let rows = List.length input in
  let cols = String.length (List.nth input 0) in
  (* let () = *)
  (*   print_string "obstacles: "; *)
  (*   print_int (Util.CMap.cardinal obstacle_map) |> print_newline; *)
  (*   print_string "rows: "; *)
  (*   print_int rows |> print_newline; *)
  (*   print_string "cols: "; *)
  (*   print_int cols |> print_newline; *)
  (*   Util.Coordinate.pp Format.std_formatter guard_coord |> print_newline *)
  (* in *)
  let coord_history =
    count_steps Util.CSet.empty (guard_coord, guard_dir) obstacle_map rows cols
  in
  Util.CSet.cardinal coord_history
in
part_one_example |> print_int |> print_newline
;;

let part_one =
  let input = Util.get_input_lines "2024/day06/input" in
  let (guard_coord, guard_dir), obstacle_map = parse input in
  let rows = List.length input in
  let cols = String.length (List.nth input 0) in
  (* let () = *)
  (*   print_string "obstacles: "; *)
  (*   print_int (Util.CMap.cardinal obstacle_map) |> print_newline; *)
  (*   print_string "rows: "; *)
  (*   print_int rows |> print_newline; *)
  (*   print_string "cols: "; *)
  (*   print_int cols |> print_newline; *)
  (*   Util.Coordinate.pp Format.std_formatter guard_coord |> print_newline *)
  (* in *)
  let coord_history =
    count_steps Util.CSet.empty (guard_coord, guard_dir) obstacle_map rows cols
  in
  Util.CSet.cardinal coord_history
in
part_one |> print_int |> print_newline

let rec is_loop set (coord, dir) map rows cols =
  (* let () = *)
  (*   print_string "Coord: "; *)
  (*   Util.Coordinate.show coord |> print_string |> print_newline *)
  (* in *)
  let repeated =
    match GuardSet.find_opt (coord, dir) set with
    | Some _ -> true
    | None -> false
  in
  if repeated
  then true
  else (
    let x, y = coord in
    if x < rows && x >= 0 && y < cols && y >= 0
    then
      is_loop (GuardSet.add (coord, dir) set) (guard_movement coord dir map) map rows cols
    else false)
;;

let part_two_example =
  let input = String.split_on_char '\n' example in
  let (guard_coord, guard_dir), obstacle_map = parse input in
  let x, y = guard_coord in
  let rows = List.length input in
  let cols = String.length (List.nth input 0) in
  let row_seq = Seq.init rows (fun x -> x) in
  let col_seq = Seq.init cols (fun x -> x) in
  Seq.fold_left
    (fun count row ->
      count
      + Seq.fold_left
          (fun count col ->
            (* let () = *)
            (*   print_string "Row Col: "; *)
            (*   print_int row; *)
            (*   print_int col |> print_newline; *)
            (*   print_string "Count: "; *)
            (*   print_int count |> print_newline *)
            (* in *)
            if x = row && y = col
            then count
            else (
              let temp_map = Util.CMap.add (row, col) `Obstacle obstacle_map in
              if is_loop
                   GuardSet.empty
                   (guard_movement guard_coord guard_dir temp_map)
                   temp_map
                   rows
                   cols
              then count + 1
              else count))
          0
          col_seq)
    0
    row_seq
in
part_two_example |> print_int |> print_newline
;;

let part_two =
  let input = Util.get_input_lines "2024/day06/input" in
  let (guard_coord, guard_dir), obstacle_map = parse input in
  let rows = List.length input in
  let cols = String.length (List.nth input 0) in
  let row_seq = Seq.init rows (fun x -> x) in
  let col_seq = Seq.init cols (fun x -> x) in
  Seq.fold_left
    (fun count row ->
      count
      + Seq.fold_left
          (fun count col ->
            let temp_map = Util.CMap.add (row, col) `Obstacle obstacle_map in
            if is_loop
                 GuardSet.empty
                 (guard_movement guard_coord guard_dir temp_map)
                 temp_map
                 rows
                 cols
            then count + 1
            else count)
          0
          col_seq)
    0
    row_seq
in
part_two |> print_int |> print_newline
