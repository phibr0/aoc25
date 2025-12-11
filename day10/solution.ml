let parse_line line =
  let re = Str.regexp "\\[\\([^]]*\\)\\]\\([^{]*\\){\\([^}]*\\)}" in
  Str.search_forward re line 0;
  let indicator = Str.matched_group 1 line in
  let buttons_str = Str.matched_group 2 line in
  let joltages_str = Str.matched_group 3 line in
  let number_list s =
    String.split_on_char ',' s |> List.map String.trim |> List.map int_of_string
  in
  let buttons =
    let re = Str.regexp "(\\([^)]*\\))" in
    let rec parse_button pos =
      try
        let _ = Str.search_forward re buttons_str pos in
        let content = Str.matched_group 1 buttons_str in
        number_list content :: parse_button (Str.match_end ())
      with Not_found -> []
    in
    parse_button 0
  in
  let joltages = number_list joltages_str in
  (indicator, buttons, joltages)

let solve_light indicator buttons =
  let len = String.length indicator in
  let target = Array.init len (fun i -> if indicator.[i] = '#' then 1 else 0) in
  let buttons_arr =
    Array.of_list
      (List.map
         (fun btn ->
           let arr = Array.make len 0 in
           List.iter (fun i -> arr.(i) <- 1) btn;
           arr)
         buttons)
  in
  let btn_count = Array.length buttons_arr in
  let min_presses = ref max_int in

  let rec search idx presses current =
    if idx = btn_count then (
      let matches = ref true in
      for i = 0 to len - 1 do
        if current.(i) mod 2 <> target.(i) then matches := false
      done;
      if !matches && presses < !min_presses then min_presses := presses)
    else (
      search (idx + 1) presses current;

      let next_state = Array.copy current in
      let btn = buttons_arr.(idx) in
      for i = 0 to len - 1 do
        next_state.(i) <- next_state.(i) + btn.(i)
      done;
      search (idx + 1) (presses + 1) next_state)
  in

  search 0 0 (Array.make len 0);
  !min_presses

let part1 input =
  input |> String.split_on_char '\n' |> List.map parse_line
  |> List.fold_left (fun acc (i, btns, _) -> acc + solve_light i btns) 0

let part2 input = 0
