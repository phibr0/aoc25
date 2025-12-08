let parse_junction_locations input =
  String.split_on_char '\n' input
  |> List.filter_map (fun line ->
         match String.split_on_char ',' line with
         | [ x; y; z ] ->
             Some (int_of_string x, int_of_string y, int_of_string z)
         | _ -> None)
  |> Array.of_list
  |> fun arr -> (arr, Array.length arr)

let calculate_sorted_distances boxes box_count =
  let edges = ref [] in
  for i = 0 to box_count - 1 do
    let x1, y1, z1 = boxes.(i) in
    for j = i + 1 to box_count - 1 do
      let x2, y2, z2 = boxes.(j) in
      let distance =
        ((x1 - x2) * (x1 - x2))
        + ((y1 - y2) * (y1 - y2))
        + ((z1 - z2) * (z1 - z2))
      in
      edges := (distance, i, j) :: !edges
    done
  done;
  List.sort (fun (d1, _, _) (d2, _, _) -> d1 - d2) !edges

let part1 input =
  let boxes, box_count = parse_junction_locations input in
  let sorted_edges = calculate_sorted_distances boxes box_count in

  let graph = Array.make box_count [] in
  let rec add_edges k l =
    if k <= 0 then ()
    else
      match l with
      | [] -> ()
      | (_, a, b) :: rest ->
          graph.(a) <- b :: graph.(a);
          graph.(b) <- a :: graph.(b);
          add_edges (k - 1) rest
  in
  add_edges 1000 sorted_edges;

  (* idea: go through each box manually.
  then for each box go to each box reachable from it.
  if a box hasnt been reached before its a new circuit *)
  let visited = Hashtbl.create box_count in
  let circuit_sizes = ref [] in

  let rec dfs box =
    Hashtbl.replace visited box true;
    List.fold_left
      (fun size neighbor ->
        if not (Hashtbl.mem visited neighbor) then size + dfs neighbor else size)
      1 graph.(box)
  in

  for i = 0 to box_count - 1 do
    if not (Hashtbl.mem visited i) then circuit_sizes := dfs i :: !circuit_sizes
  done;

  let sorted_sizes = List.sort (fun a b -> b - a) !circuit_sizes in
  match sorted_sizes with a :: b :: c :: _ -> a * b * c | _ -> failwith "?"

let part2 input =
  let boxes, box_count = parse_junction_locations input in
  let sorted_edges = calculate_sorted_distances boxes box_count in
  let circuits = Array.init box_count (fun i -> i) in
  let circuit_count = ref box_count in

  let rec solve l =
    match l with
    | [] -> 0
    | (_, a, b) :: rest ->
        let circuit_a = circuits.(a) in
        let circuit_b = circuits.(b) in

        if circuit_a <> circuit_b then (
          for k = 0 to box_count - 1 do
            if circuits.(k) = circuit_b then circuits.(k) <- circuit_a
          done;

          decr circuit_count;

          if !circuit_count = 1 then
            let x1, _, _ = boxes.(a) in
            let x2, _, _ = boxes.(b) in
            x1 * x2
          else solve rest)
        else solve rest
  in

  solve sorted_edges
