module Coordinate = struct
  type t = int * int [@@deriving show]

  let adj (x, y) = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]
  let diag (x, y) = [ x - 1, y - 1; x + 1, y - 1; x - 1, y + 1; x + 1, y + 1 ]
  let adj_diag (x, y) = adj (x, y) @ diag (x, y)
  let compare = compare
  let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
end

module CMap = struct
  include Map.Make (Coordinate)

  let pp pp' fmt =
    iter (fun key value -> Format.fprintf fmt "%a -> %a\n" Coordinate.pp key pp' value)
  ;;
end

module CSet = struct
  include Set.Make (Coordinate)

  let pp fmt set =
    Format.pp_print_string fmt "{";
    Format.pp_print_seq
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
      Coordinate.pp
      fmt
      (to_seq set)
  ;;
end
