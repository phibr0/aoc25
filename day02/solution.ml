let parse range =
  String.split_on_char '-' range |> List.map int_of_string |> function
  | [ a; b ] -> (a, b)
  | _ -> failwith "Invalid input"

let countInvalidIds validatorFn input =
  String.split_on_char ',' input
  |> List.map parse
  |> List.concat_map (fun (start, finish) ->
         List.init (finish - start + 1) (fun i -> start + i))
  |> List.filter (fun id -> not (validatorFn id))
  |> List.fold_left (fun sum x -> sum + x) 0

let part1 input =
  let isValid id =
    string_of_int id |> fun idStr ->
    let length = String.length idStr in
    if length mod 2 = 0 then
      let endPart = String.sub idStr (length / 2) (length / 2) in
      let startPart = String.sub idStr 0 (length / 2) in
      startPart <> endPart
    else true
  in
  countInvalidIds isValid input

let part2 input =
  let isValid id =
    let idStr = string_of_int id in
    let length = String.length idStr in
    let divisors =
      List.init (length - 1) (fun i -> i + 1)
      |> List.filter (fun i -> length mod i = 0)
    in
    let rec hasRepeatedSequence seqLengths =
      match seqLengths with
      | [] -> false
      | len :: rest ->
          let segment = String.sub idStr 0 len in
          let rec checkFromIndex index =
            if index + len > length then true
            else
              let currentSegment = String.sub idStr index len in
              if currentSegment <> segment then false
              else checkFromIndex (index + len)
          in
          if checkFromIndex 0 then true else hasRepeatedSequence rest
    in
    not (hasRepeatedSequence divisors)
  in
  countInvalidIds isValid input
