let split_ws s = String.split_on_char ' ' s |> List.filter (( <> ) "")

let parse_lines input =
  let lines = String.split_on_char '\n' input |> Array.of_list in
  let n = Array.length lines in
  if n = 0 then ([||], [||])
  else (Array.sub lines 0 (n - 1), split_ws lines.(n - 1) |> Array.of_list)

let parse_grid input =
  let lines, ops = parse_lines input in
  let grid =
    Array.map
      (fun s -> split_ws s |> List.map int_of_string |> Array.of_list)
      lines
  in
  let h = Array.length grid in
  let w = if h > 0 then Array.length grid.(0) else 0 in
  (grid, ops, w, h)

let part1 input =
  let grid, ops, w, h = parse_grid input in
  let calc_col x =
    let op =
      match ops.(x) with "+" -> ( + ) | "*" -> ( * ) | _ -> failwith "Bad op"
    in
    let rec loop y acc =
      if y = h then acc else loop (y + 1) (op acc grid.(y).(x))
    in
    if h = 0 then 0 else loop 1 grid.(0).(x)
  in
  let rec loop x acc = if x = w then acc else loop (x + 1) (acc + calc_col x) in
  loop 0 0

let get_token_bounds line =
  let len = String.length line in
  let rec loop i acc =
    if i >= len then List.rev acc
    else if line.[i] = ' ' then loop (i + 1) acc
    else
      let start = i in
      let rec find_end j =
        if j < len && line.[j] <> ' ' then find_end (j + 1) else j
      in
      let stop = find_end (i + 1) in
      loop stop ((start, stop) :: acc)
  in
  Array.of_list (loop 0 [])

let part2 input =
  let lines, ops = parse_lines input in
  let bounds = Array.map get_token_bounds lines in
  let w = Array.length ops in
  let total = ref 0 in

  for col = 0 to w - 1 do
    let min_s, max_e =
      Array.fold_left
        (fun (mins, maxe) row ->
          let s, e = row.(col) in
          (min mins s, max maxe e))
        (max_int, 0) bounds
    in
    let op =
      match ops.(col) with
      | "+" -> ( + )
      | "*" -> ( * )
      | _ -> failwith "Bad op"
    in
    let inner = ref 0 in
    let first = ref true in

    for x = max_e - 1 downto min_s do
      let buf = Buffer.create (Array.length lines) in
      Array.iter
        (fun l ->
          if x < String.length l && l.[x] <> ' ' then Buffer.add_char buf l.[x])
        lines;
      let s = Buffer.contents buf in
      if s <> "" then
        let n = int_of_string s in
        if !first then (
          inner := n;
          first := false)
        else inner := op !inner n
    done;
    total := !total + !inner
  done;
  !total
