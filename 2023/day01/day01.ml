open String

let () =
  In_channel.input_lines (In_channel.open_text "2023/day01/day01.txt")
  |> List.map (fun x ->
    to_seq x
    |> Seq.filter (fun x -> '0' <= x && x <= '9')
    |> of_seq
    |> fun x ->
    let num x = Char.(code x - code '0') in
    match length x with
    | 0 -> 0
    | n -> (num (get x 0) * 10) + num (get x (n - 1)))
  |> List.fold_left ( + ) 0
  |> print_int
  |> print_newline
;;

let () =
  In_channel.input_lines (In_channel.open_text "2023/day01/day01.txt")
  |> List.map (fun x ->
    List.fold_left
      (fun acc (a, b) -> Re.replace_string (Re.compile (Re.str a)) ~by:b acc)
      x
      [ "zero", "z0o"
      ; "one", "o1e"
      ; "two", "t2o"
      ; "three", "t3e"
      ; "four", "f4r"
      ; "five", "f5e"
      ; "six", "s6x"
      ; "seven", "s7n"
      ; "eight", "e8t"
      ; "nine", "n9e"
      ]
    |> to_seq
    |> Seq.filter (fun x -> '0' <= x && x <= '9')
    |> of_seq
    |> fun x ->
    let num x = Char.(code x - code '0') in
    match length x with
    | 0 -> 0
    | n -> (num (get x 0) * 10) + num (get x (n - 1)))
  |> List.fold_left ( + ) 0
  |> print_int
;;
