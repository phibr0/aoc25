let parse_grid input =
  input |> String.split_on_char '\n'
  |> List.map (fun line -> String.to_seq line |> Array.of_seq)
  |> Array.of_list

module PositionSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let part1 input =
  let grid = parse_grid input in
  let bounds_x, bounds_y = (Array.length grid.(0), Array.length grid) in
  let start_x = Array.find_index (( = ) 'S') grid.(0) |> Option.get in
  let visited = ref PositionSet.empty in
  let rec simulate (x, y) =
    if PositionSet.mem (x, y) !visited then PositionSet.empty
    else (
      visited := PositionSet.add (x, y) !visited;
      if y >= bounds_y || x < 0 || x >= bounds_x then PositionSet.empty
      else
        match grid.(y).(x) with
        | '.' | 'S' -> simulate (x, y + 1)
        | '^' ->
            let left = simulate (x - 1, y) in
            let right = simulate (x + 1, y) in
            PositionSet.add (x, y) (PositionSet.union left right)
        | _ -> failwith "Unexpected character")
  in
  simulate (start_x, 0) |> PositionSet.cardinal

(* https://ocaml.org/docs/memoization *)
let memo_rec f =
  let h = Hashtbl.create 64 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g

let part2 input =
  let grid = parse_grid input in
  let bounds_x, bounds_y = (Array.length grid.(0), Array.length grid) in
  let start_x = Array.find_index (( = ) 'S') grid.(0) |> Option.get in
  let simulate next (x, y) =
    if y >= bounds_y || x < 0 || x >= bounds_x then 0
    else
      match grid.(y).(x) with
      | '.' | 'S' -> next (x, y + 1)
      | '^' ->
          let left = next (x - 1, y) in
          let right = next (x + 1, y) in
          1 + left + right
      | _ -> failwith "Unexpected character"
  in
  (memo_rec simulate) (start_x, 0) + 1
