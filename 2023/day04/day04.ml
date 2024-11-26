let parse_line line =
  let open Angstrom in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let space = take_while1 (( = ) ' ') in
  let separator = space *> string "|" <* space in
  let prefix =
    lift3
      (fun _ card_no _ -> card_no)
      (string "Card" <* space)
      integer
      (string ":" <* space)
  in
  let numbers =
    lift3
      (fun winners _ mine -> winners, mine)
      (sep_by1 space integer)
      separator
      (sep_by1 space integer)
  in
  match
    parse_string
      ~consume:All
      (lift2 (fun card_no (winners, card) -> card_no, (winners, card)) prefix numbers)
      line
  with
  | Ok (card_no, (winners, card)) -> card_no, (winners, card)
  | Error msg -> failwith msg
;;

let () =
  let input = In_channel.input_lines (In_channel.open_text "2023/day04/input") in
  List.fold_left
    (fun sum line ->
      let _, (winning_numbers, numbers) = parse_line line in
      let count =
        List.fold_left
          (fun count number ->
            if List.mem number winning_numbers then count + 1 else count)
          0
          numbers
      in
      let rec mult2 acc n = if n = 1 then acc else mult2 (acc * 2) (n - 1) in
      if count > 1 then sum + mult2 1 count else sum + count)
    0
    input
  |> print_int
  |> print_newline
;;

let () =
  let input = In_channel.input_lines (In_channel.open_text "2023/day04/input") in
  let module Int_map = Map.Make (Int) in
  let cards = List.map parse_line input in
  let winners =
    List.map
      (fun (card_no, (winning_numbers, numbers)) ->
        let count =
          List.fold_left
            (fun count number ->
              if List.mem number winning_numbers then count + 1 else count)
            0
            numbers
        in
        card_no, count)
      cards
  in
  let copies =
    List.fold_left
      (fun map (card_no, count) ->
        let map =
          Int_map.update
            card_no
            (function
              | Some num -> Some (num + 1)
              | None -> Some 1)
            map
        in
        let won_card_ids = List.init count (fun i -> i + 1 + card_no) in
        List.fold_left
          (fun map id ->
            let x =
              match Int_map.find_opt card_no map with
              | Some x -> x
              | None -> 0
            in
            Int_map.update
              id
              (function
                | Some num -> Some (num + x)
                | None -> Some x)
              map)
          map
          won_card_ids)
      Int_map.empty
      winners
  in
  Int_map.fold (fun _ copies sum -> sum + copies) copies 0 |> print_int |> print_newline
;;
