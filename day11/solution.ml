let parse input =
  String.split_on_char '\n' input
  |> List.map (fun line ->
         match String.split_on_char ':' line with
         | [ node; neighbors ] -> (node, String.split_on_char ' ' neighbors)
         | _ -> failwith "?")
  |> List.to_seq |> Hashtbl.of_seq

let count_paths adj start goal =
  let memo = Hashtbl.create 64 in
  let visiting = Hashtbl.create 16 in
  let rec solve current =
    if current = goal then 1
    else if Hashtbl.mem visiting current then 0
    else
      match Hashtbl.find_opt memo current with
      | Some count -> count
      | None -> (
          Hashtbl.add visiting current true;
          match Hashtbl.find_opt adj current with
          | Some n ->
              let count = List.fold_left (fun a next -> a + solve next) 0 n in
              Hashtbl.remove visiting current;
              Hashtbl.add memo current count;
              count
          | None -> 0)
  in
  solve start

let part1 input =
  let adj = parse input in
  count_paths adj "you" "out"

let part2 input =
  let adj = parse input in

  let svr_fft = count_paths adj "svr" "fft" in
  let fft_dac = count_paths adj "fft" "dac" in
  let dac_out = count_paths adj "dac" "out" in

  let svr_dac = count_paths adj "svr" "dac" in
  let dac_fft = count_paths adj "dac" "fft" in
  let fft_out = count_paths adj "fft" "out" in

  (svr_fft * fft_dac * dac_out) + (svr_dac * dac_fft * fft_out)
