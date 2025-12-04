let parse_grid input =
  String.split_on_char '\n' input |> Array.of_list |> fun lines ->
  let grid =
    Array.map (fun s -> Array.init (String.length s) (String.get s)) lines
  in
  let height = Array.length grid in
  let width = if height > 0 then Array.length grid.(0) else 0 in
  (grid, width, height)

let is_paper grid width height x y =
  if x < 0 || y < 0 || x >= width || y >= height then false
  else grid.(y).(x) = '@'

let count_neighbors grid width height x y =
  let count = ref 0 in
  for dy = -1 to 1 do
    for dx = -1 to 1 do
      if (dx <> 0 || dy <> 0) && is_paper grid width height (x + dx) (y + dy)
      then incr count
    done
  done;
  !count

let iter_grid width height f =
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      f x y
    done
  done

let part1 input =
  let grid, width, height = parse_grid input in
  let accessible_count = ref 0 in
  iter_grid width height (fun x y ->
      if
        is_paper grid width height x y
        && count_neighbors grid width height x y < 4
      then incr accessible_count);
  !accessible_count

let part2 input =
  let grid, width, height = parse_grid input in
  let total_accessible = ref 0 in
  let changed = ref true in
  while !changed do
    let to_remove = ref [] in
    iter_grid width height (fun x y ->
        if
          is_paper grid width height x y
          && count_neighbors grid width height x y < 4
        then (
          to_remove := (x, y) :: !to_remove;
          incr total_accessible));
    match !to_remove with
    | [] -> changed := false
    | points -> List.iter (fun (x, y) -> grid.(y).(x) <- '.') points
  done;
  !total_accessible
