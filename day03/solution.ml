let solve_bank length bank =
  let len = String.length bank in
  let rec solve_bank_inner idx rem acc =
    if rem = 0 then acc
    else
      let limit = len - rem in
      let rec find_max_after index best_digit best_index =
        if index > limit then (best_digit, best_index)
        else
          let digit = int_of_string (String.make 1 bank.[index]) in
          if digit > best_digit then find_max_after (index + 1) digit index
          else find_max_after (index + 1) best_digit best_index
      in
      let digit, next_idx = find_max_after idx (-1) (-1) in
      solve_bank_inner (next_idx + 1) (rem - 1) ((acc * 10) + digit)
  in
  solve_bank_inner 0 length 0

let part1 input =
  String.split_on_char '\n' input
  |> List.map (solve_bank 2)
  |> List.fold_left ( + ) 0

let part2 input =
  String.split_on_char '\n' input
  |> List.map (solve_bank 12)
  |> List.fold_left ( + ) 0
