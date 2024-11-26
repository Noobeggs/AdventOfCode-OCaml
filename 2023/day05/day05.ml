type mapping =
  { dest : int
  ; source : int
  ; range : int
  }

let parse input =
  let open Angstrom in
  let integer =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let numbers = sep_by1 (char ' ') integer in
  let seeds = string "seeds: " *> numbers <* string "\n\n" in
  let map_header = take_while1 (( <> ) '\n') *> char '\n' in
  let map_line =
    lift3
      (fun dest source range -> { dest; source; range })
      (integer <* char ' ')
      (integer <* char ' ')
      integer
  in
  let map = map_header *> sep_by1 (char '\n') map_line in
  let maps = sep_by1 (string "\n\n") map in
  let parser = lift2 (fun seeds maps -> seeds, maps) seeds maps <* char '\n' in
  (* Depending on how you copy the input might need to parse an extra newline char *)
  match parse_string ~consume:All parser input with
  | Ok v -> v
  | Error msg -> failwith msg
;;

let range { dest; source; range } x =
  if x >= source && x < source + range then Some (dest + (x - source)) else None
;;

let map_seeds_to_location seeds maps =
  let map' source map = List.find_map (fun map_line -> range map_line source) map in
  let map_seed seed =
    List.fold_left
      (fun acc map ->
        match map' acc map with
        | Some x -> x
        | None -> acc)
      seed
      maps
  in
  List.map map_seed seeds
;;

let () =
  let input = In_channel.input_all (In_channel.open_text "2023/day05/input") in
  let seeds, maps = parse input in
  let locations = map_seeds_to_location seeds maps in
  List.fold_left (fun acc location -> Int.min acc location) Int.max_int locations
  |> print_int
  |> print_newline
;;

let () =
  let input = In_channel.input_all (In_channel.open_text "2023/day05/input") in
  let seeds, maps = parse input in
  let rec seeds_range min = function
    | start :: range :: tl ->
      let seq = Seq.init range (fun x -> start + x) in
      let s_min =
        Seq.fold_left
          (fun acc seed ->
            let location = map_seeds_to_location [ seed ] maps in
            Int.min acc (List.hd location))
          min
          seq
      in
      seeds_range (Int.min min s_min) tl
    | _ -> min
  in
  seeds_range Int.max_int seeds |> print_int |> print_newline
;;
