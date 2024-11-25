let parse_line line =
  let open Angstrom in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| fun number -> `Number number
  in
  let nothing = char '.' *> return `Nothing in
  let gear = char '*' *> return `Gear in
  let symbol = any_char *> return `Symbol in
  Angstrom.parse_string
    ~consume:All
    (integer <|> nothing <|> gear <|> symbol |> many)
    line
  |> Result.get_ok
;;

let parse input =
  let tokens = List.map parse_line input in
  let symbols = Util.CMap.empty in
  let numbers = Util.CMap.empty in
  let _row, symbols, numbers =
    List.fold_left
      (fun (row, symbols, numbers) tokens ->
        let _col, symbols, numbers =
          List.fold_left
            (fun (col, symbols, numbers) -> function
              | `Nothing -> col + 1, symbols, numbers
              | (`Gear | `Symbol) as symbol ->
                let symbols = Util.CMap.add (row, col) symbol symbols in
                col + 1, symbols, numbers
              | `Number number ->
                let numbers = Util.CMap.add (row, col) number numbers in
                col + String.length number, symbols, numbers)
            (0, symbols, numbers)
            tokens
        in
        row + 1, symbols, numbers)
      (0, symbols, numbers)
      tokens
  in
  symbols, numbers
;;

let adj_cset symbols =
  Util.CMap.fold
    (fun coord _ adj_cset ->
      Util.CSet.add_seq (List.to_seq (Util.Coordinate.adj_diag coord)) adj_cset)
    symbols
    Util.CSet.empty
;;

let () =
  let input = In_channel.input_lines (In_channel.open_text "2023/day03/input") in
  let symbols, numbers = parse input in
  let symbol_adj = adj_cset symbols in
  let part_numbers_sum =
    Util.CMap.fold
      (fun coord number sum ->
        let coords =
          List.init (String.length number) (fun i -> Util.Coordinate.(coord + (0, i)))
        in
        if List.exists (fun coord -> Util.CSet.mem coord symbol_adj) coords
        then sum + int_of_string number
        else sum)
      numbers
      0
  in
  print_int part_numbers_sum |> print_newline
;;

let () =
  let input = In_channel.input_lines (In_channel.open_text "2023/day03/input") in
  let symbols, numbers = parse input in
  let gears =
    Util.CMap.filter_map
      (fun _ -> function
        | `Gear -> Some []
        | _ -> None)
      symbols
  in
  let gears =
    Util.CMap.fold
      (fun coord number gears ->
        let coords =
          List.init (String.length number) (fun i -> Util.Coordinate.(coord + (0, i)))
        in
        let number = int_of_string number in
        (* Coordinates to check for gears *)
        let adj_gears =
          List.fold_left
            (fun adj_gears coord ->
              let adj_coords = Util.Coordinate.adj_diag coord in
              List.fold_left
                (fun adj_gears coord -> Util.CSet.add coord adj_gears)
                adj_gears
                adj_coords)
            Util.CSet.empty
            coords
        in
        Util.CSet.fold
          (fun coord gears ->
            Util.CMap.update
              coord
              (function
                | Some list -> Some (number :: list)
                | _ -> None)
              gears)
          adj_gears
          gears)
      numbers
      gears
  in
  let gear_ratio =
    Util.CMap.fold
      (fun _ adj_numbers sum ->
        match adj_numbers with
        | [ one; two ] -> sum + (one * two)
        | _ -> sum)
      gears
      0
  in
  print_int gear_ratio |> print_newline
;;
