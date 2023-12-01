let word_digits =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let prefix_to_digit s =
  let rec aux s word_digits =
    match word_digits with
    | [] -> None
    | (word, digit) :: t ->
        if String.starts_with ~prefix:word s then Some digit else aux s t
  in
  aux s word_digits

(* Excluding 0 *)
let digit_of_char c =
  let c_code = int_of_char c in
  let zero_code = int_of_char '0' in
  if zero_code < c_code && c_code <= int_of_char '9' then
    Some (c_code - zero_code)
  else None

(* Set `process_words` to `true` for part 2 *)
let process_line ~process_words s =
  let len = String.length s in
  let rec aux s start_idx acc last_digit =
    if start_idx >= len then (acc * 10) + Option.get last_digit
    else
      let c = s.[start_idx] in
      let digit =
        Util.or_else (digit_of_char c) (fun () ->
            if process_words then
              let s' = String.sub s start_idx (len - start_idx) in
              prefix_to_digit s'
            else None)
      in
      match digit with
      | None -> aux s (start_idx + 1) acc last_digit
      | Some digit -> (
          match last_digit with
          | None -> aux s (start_idx + 1) (acc + digit) (Some digit)
          | Some _ -> aux s (start_idx + 1) acc (Some digit))
  in
  aux s 0 0 None

let process ~process_words ch =
  let rec aux ch acc =
    try
      let calibration = process_line ~process_words (input_line ch) in
      aux ch (acc + calibration)
    with End_of_file -> acc
  in
  aux ch 0
