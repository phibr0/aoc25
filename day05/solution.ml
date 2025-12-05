let parse_input input =
  match String.split_on_char '#' input with
  | [ ranges; ingredients ] ->
      let ranges =
        String.split_on_char '\n' ranges
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> List.map (fun range ->
               match String.split_on_char '-' range with
               | [ start_str; end_str ] ->
                   (int_of_string start_str, int_of_string end_str)
               | _ -> failwith "Invalid range format")
      in
      let ingredients =
        String.split_on_char '\n' ingredients
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> List.map int_of_string
      in
      (ranges, ingredients)
  | _ -> failwith "Invalid input format"

let part1 input =
  let ranges, ingredients = parse_input input in
  List.filter
    (fun value ->
      List.exists
        (fun (start_val, end_val) -> value >= start_val && value <= end_val)
        ranges)
    ingredients
  |> List.length

let part2 input =
  let ranges, _ = parse_input input in
  let sorted_ranges = List.sort (fun (a, _) (b, _) -> compare a b) ranges in
  let rec merge_ranges acc current_left current_right = function
    | [] -> (current_left, current_right) :: acc
    | (left, right) :: rest ->
        if left <= current_right + 1 then (* overlap *)
          merge_ranges acc current_left (max current_right right) rest
        else (* new range *)
          merge_ranges ((current_left, current_right) :: acc) left right rest
  in
  let merged_ranges =
    match sorted_ranges with
    | [] -> []
    | (left, right) :: rest -> merge_ranges [] left right rest
  in
  List.fold_left
    (fun acc (left, right) -> acc + (right - left + 1))
    0 merged_ranges
