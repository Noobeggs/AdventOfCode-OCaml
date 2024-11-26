let parse input =
  let open Angstrom in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let space = take_while1 (( = ) ' ') in
  let label = take_while (( <> ) ':') <* char ':' <* space in
  let numbers = sep_by1 space integer in
  let p =
    lift2
      (fun time distance -> time, distance)
      (label *> numbers <* char '\n')
      (label *> numbers)
  in
  parse_string ~consume:All p input |> Result.get_ok
;;

let () =
  let input = In_channel.input_all (In_channel.open_text "2023/day06/input") in
  let times, distances = parse input in
  let can_win hold_time race_time distance =
    if (race_time - hold_time) * hold_time >= distance then true else false
  in
  List.fold_left
    (fun sum (time, distance) ->
      let hold_times = Seq.init time (fun x -> x) in
      let win_count =
        Seq.fold_left
          (fun sum hold_time -> if can_win hold_time time distance then sum + 1 else sum)
          0
          hold_times
      in
      sum * win_count)
    1
    (List.combine times distances)
  |> print_int
  |> print_newline
;;

let parse2 input =
  let open Angstrom in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  let space = take_while1 (( = ) ' ') in
  let label = take_while (( <> ) ':') <* char ':' <* space in
  let numbers = sep_by1 space integer in
  let p =
    lift2
      (fun time distance -> time, distance)
      (label *> numbers <* char '\n')
      (label *> numbers)
  in
  parse_string ~consume:All p input |> Result.get_ok
;;

let () =
  let input = In_channel.input_all (In_channel.open_text "2023/day06/input") in
  let times, distances = parse2 input in
  let time = int_of_string (String.concat "" times) in
  let distance = int_of_string (String.concat "" distances) in
  let can_win hold_time race_time distance =
    if (race_time - hold_time) * hold_time >= distance then true else false
  in
  let hold_times = Seq.init time (fun x -> x) in
  Seq.fold_left
    (fun sum hold_time -> if can_win hold_time time distance then sum + 1 else sum)
    0
    hold_times
  |> print_int
  |> print_newline
;;
