let ( % ) x y = ((x mod y) + y) mod y

let parse line =
  let dir = String.get line 0 in
  let amount = int_of_string (String.sub line 1 (String.length line - 1)) in
  (dir, amount)

let part1 input =
  String.split_on_char '\n' input
  |> List.map parse
  |> List.fold_left
       (fun (acc, counter) (dir, amount) ->
         let delta = if dir = 'L' then -amount else amount in
         let rotation = (acc + delta) % 100 in
         (rotation, counter + Bool.to_int (rotation == 0)))
       (50, 0)
  |> snd

let part2 input =
  let countOver0 dir amount current =
    match dir with
    | 'R' -> (current + amount) / 100
    | 'L' ->
        if amount < current then 0
        else if current = 0 then amount / 100
        else ((amount - current) / 100) + 1
    | _ -> failwith "Invalid direction"
  in
  String.split_on_char '\n' input
  |> List.map parse
  |> List.fold_left
       (fun (acc, counter) (dir, amount) ->
         let delta = if dir = 'L' then -amount else amount in
         let rotationsOver0 = countOver0 dir amount acc in
         ((acc + delta) % 100, counter + rotationsOver0))
       (50, 0)
  |> snd
