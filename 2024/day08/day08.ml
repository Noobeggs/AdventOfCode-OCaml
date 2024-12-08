let example =
  "............\n\
   ........0...\n\
   .....0......\n\
   .......0....\n\
   ....0.......\n\
   ......A.....\n\
   ............\n\
   ............\n\
   ........A...\n\
   .........A..\n\
   ............\n\
   ............"
;;

type nodes =
  | Nothing
  | Antenna of char

module Char_map = Map.Make (Char)

(* type char_to_coords_map = Util.Coordinate.t list Char_map.t [@@deriving.show] *)

let parse input =
  let open Angstrom in
  let nothing = char '.' *> return Nothing in
  let antenna = not_char '\n' >>| fun c -> Antenna c in
  let p = sep_by1 (char '\n') (nothing <|> antenna |> many) in
  let nodes = parse_string ~consume:All p input |> Result.get_ok in
  let () =
    print_string "Node list length: ";
    List.length nodes |> print_int |> print_newline
  in
  let antenna_map = Char_map.empty in
  List.fold_left
    (fun (row_num, _, map) line ->
      let cols, map =
        List.fold_left
          (fun (col_num, map) -> function
            | Nothing -> col_num + 1, map
            | Antenna c ->
              (* let () = *)
              (*   print_int row_num; *)
              (*   print_string " "; *)
              (*   print_int col_num |> print_newline *)
              (* in *)
              col_num + 1, Char_map.add_to_list c (row_num, col_num) map)
          (0, map)
          line
      in
      row_num + 1, cols, map)
    (0, 0, antenna_map)
    nodes
;;

let antinodes a b : Util.Coordinate.t * Util.Coordinate.t =
  let open Util.Coordinate in
  let ab = b + -a in
  b + ab, a + -ab
;;

let rec combinations set node rows cols = function
  | [] -> set
  | hd :: tl ->
    let set =
      (* let () = print_string (Util.Coordinate.show node) |> print_newline in *)
      List.fold_left
        (fun cset lst_node ->
          let first, second = antinodes node lst_node in
          let x, y = first in
          let cset =
            if x < rows && x >= 0 && y < cols && y >= 0
            then Util.CSet.add first cset
            else cset
          in
          let x, y = second in
          if x < rows && x >= 0 && y < cols && y >= 0
          then Util.CSet.add second cset
          else cset)
        set
        (hd :: tl)
    in
    combinations set hd rows cols tl
;;

let part_one_example =
  let input = example in
  let rows, cols, antenna_map = parse input in
  let antinodes =
    Char_map.fold
      (fun _ coords antinodes ->
        match coords with
        | x :: y :: tl -> combinations antinodes x rows cols (y :: tl)
        | _ -> antinodes)
      antenna_map
      Util.CSet.empty
  in
  (* let () = Util.CSet.pp Format.std_formatter antinodes |> print_newline in *)
  Util.CSet.cardinal antinodes
in
part_one_example |> print_int |> print_newline
;;

let part_one =
  let input = Util.get_input "2024/day08/input" in
  let rows, cols, antenna_map = parse input in
  let antinodes =
    Char_map.fold
      (fun _ coords antinodes ->
        match coords with
        | x :: y :: tl -> combinations antinodes x rows cols (y :: tl)
        | _ -> antinodes)
      antenna_map
      Util.CSet.empty
  in
  (* let () = Util.CSet.pp Format.std_formatter antinodes |> print_newline in *)
  Util.CSet.cardinal antinodes
in
part_one |> print_int |> print_newline

let antinodes_extrapolated (a : Util.Coordinate.t) (b : Util.Coordinate.t) rows cols =
  let open Util.Coordinate in
  let ab = b + -a in
  let rec extrapolate lst a b ab =
    (* let () = *)
    (*   Util.Coordinate.show a |> print_string; *)
    (*   print_string " "; *)
    (*   Util.Coordinate.show b |> print_string |> print_newline *)
    (* in *)
    let c, d = a + -ab in
    (* let () = Util.Coordinate.show (c, d) |> print_string |> print_newline in *)
    let within_bounds_cd = c < rows && c >= 0 && d < cols && d >= 0 in
    let lst = if within_bounds_cd then (c, d) :: lst else lst in
    let e, f = b + ab in
    (* let () = Util.Coordinate.show (e, f) |> print_string |> print_newline in *)
    let within_bounds_ef = e < rows && e >= 0 && f < cols && f >= 0 in
    let lst = if within_bounds_ef then (e, f) :: lst else lst in
    if within_bounds_cd || within_bounds_ef then extrapolate lst (c, d) (e, f) ab else lst
  in
  extrapolate [ a; b ] a b ab
;;

let rec combinations2 set node rows cols = function
  | [] -> set
  | hd :: tl ->
    let set =
      (* let () = print_string (Util.Coordinate.show node) |> print_newline in *)
      List.fold_left
        (fun cset lst_node ->
          let antinodes_lst = antinodes_extrapolated node lst_node rows cols in
          List.fold_left
            (fun cset antinode -> Util.CSet.add antinode cset)
            cset
            antinodes_lst)
        set
        (hd :: tl)
    in
    combinations2 set hd rows cols tl
;;

let part_two_example =
  let input = example in
  let rows, cols, antenna_map = parse input in
  let antinodes =
    Char_map.fold
      (fun _ coords antinodes ->
        match coords with
        | x :: y :: tl -> combinations2 antinodes x rows cols (y :: tl)
        | _ -> antinodes)
      antenna_map
      Util.CSet.empty
  in
  let () = Util.CSet.pp Format.std_formatter antinodes |> print_newline in
  Util.CSet.cardinal antinodes
in
part_two_example |> print_int |> print_newline
;;

let part_two =
  let input = Util.get_input "2024/day08/input" in
  let rows, cols, antenna_map = parse input in
  let antinodes =
    Char_map.fold
      (fun _ coords antinodes ->
        match coords with
        | x :: y :: tl -> combinations2 antinodes x rows cols (y :: tl)
        | _ -> antinodes)
      antenna_map
      Util.CSet.empty
  in
  (* let () = Util.CSet.pp Format.std_formatter antinodes |> print_newline in *)
  Util.CSet.cardinal antinodes
in
part_two |> print_int |> print_newline
