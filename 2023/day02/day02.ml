type colors =
  | Red
  | Green
  | Blue
(* [@@deriving show] *)

type draw = (int * colors) list (* [@@deriving show] *)

type game =
  { id : int
  ; draws : draw list
  }
(* [@@deriving show] *)

let parse_game s =
  let open Angstrom in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let color =
    string "red" *> return Red
    <|> string "green" *> return Green
    <|> string "blue" *> return Blue
  in
  let game_number = string "Game " *> integer <* string ": " in
  let cube = lift3 (fun n _ c -> n, c) integer (string " ") color in
  let hand = sep_by1 (string ", ") cube in
  let hands = sep_by1 (string "; ") hand in
  let game_record = lift2 (fun id draws -> { id; draws }) game_number hands in
  match parse_string ~consume:All game_record s with
  | Ok v -> v
  | Error msg -> failwith msg
;;

let () =
  In_channel.input_lines (In_channel.open_text "2023/day02/input.txt")
  |> List.map parse_game
  |> List.map (fun game ->
    game.draws
    |> List.to_seq
    |> Seq.for_all (fun draw ->
      List.to_seq draw
      |> Seq.for_all (fun input ->
        match input with
        | n, Red -> n <= 12
        | n, Green -> n <= 13
        | n, Blue -> n <= 14))
    |> fun is_valid -> game.id, is_valid)
  |> List.fold_left (fun acc (id, is_valid) -> if is_valid then acc + id else acc) 0
  |> print_int
  |> print_newline
;;

(* let min_required draw = List.fold_left (fun (r,g,b) input -> match input with
   | n, Red -> (r+n, g, b)
   | n, Green -> (r, g+n, b)
   | n, Blue -> (r, g, b+n)) draw *)

let () =
  In_channel.input_lines (In_channel.open_text "2023/day02/input.txt")
  |> List.map parse_game
  |> List.map (fun game ->
    game.draws
    |> List.map
         (List.fold_left
            (fun (r, g, b) input ->
              match input with
              | n, Red -> r + n, g, b
              | n, Green -> r, g + n, b
              | n, Blue -> r, g, b + n)
            (0, 0, 0))
    |> List.fold_left (fun (r, g, b) (x, y, z) -> max r x, max g y, max b z) (0, 0, 0))
  |> List.fold_left (fun acc (r, g, b) -> acc + (r * g * b)) 0
  |> print_int
  |> print_newline
;;
