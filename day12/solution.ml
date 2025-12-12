let parse input =
  String.split_on_char '\n' input
  |> List.map (fun line ->
         match String.split_on_char ':' line with
         | [ dims; counts_str ] ->
             let dim_parts = String.split_on_char 'x' (String.trim dims) in
             let width = int_of_string (List.nth dim_parts 0) in
             let height = int_of_string (List.nth dim_parts 1) in
             let counts =
               String.trim counts_str |> String.split_on_char ' '
               |> List.map int_of_string
             in
             (width, height, counts)
         | _ -> failwith "?")

let part1 input =
  parse input
  |> List.filter (fun (w, h, counts) ->
         List.fold_left (fun acc count -> acc + (count * 3 * 3)) 0 counts
         <= w * h)
  |> List.length

let part2 input = -1
