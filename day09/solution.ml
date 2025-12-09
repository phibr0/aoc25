let area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

let parse input =
  String.split_on_char '\n' input
  |> List.map (fun line ->
         match String.split_on_char ',' line with
         | [ x; y ] -> (int_of_string x, int_of_string y)
         | _ -> failwith "Invalid point format")

let part1 input =
  let points = parse input in
  let rec max_area = function
    | [] | [ _ ] -> 0
    | p1 :: r -> List.fold_left (fun a p2 -> max a (area p1 p2)) (max_area r) r
  in
  max_area points

let overlap (x1, y1) (x2, y2) edges =
  let min_x, max_x = (min x1 x2, max x1 x2) in
  let min_y, max_y = (min y1 y2, max y1 y2) in
  let check ((ex1, ey1), (ex2, ey2)) =
    let e_min_x = min ex1 ex2 in
    let e_max_x = max ex1 ex2 in
    let e_min_y = min ey1 ey2 in
    let e_max_y = max ey1 ey2 in
    e_max_x <= min_x || e_min_x >= max_x || e_max_y <= min_y || e_min_y >= max_y
  in
  List.exists (fun edge -> not (check edge)) edges

let edges points =
  match points with
  | [] -> []
  | h :: _ ->
      let rec slide = function
        | [] | [ _ ] -> []
        | x :: y :: rest -> (x, y) :: slide (y :: rest)
      in
      slide (points @ [ h ])

let part2 input =
  let points = parse input in
  let outer_edges = edges points in
  let rec max_area = function
    | [] | [ _ ] -> 0
    | p1 :: rest ->
        List.fold_left
          (fun prev_area p2 ->
            let new_area = area p1 p2 in
            if new_area > prev_area && not (overlap p1 p2 outer_edges) then
              new_area
            else prev_area)
          (max_area rest) rest
  in
  max_area points
