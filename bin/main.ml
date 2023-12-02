let () =
  open_in "input/day2.txt"
  (* Aoc2023.Day1.process ~part:2 *)
  |> Aoc2023.Day2.process ~part:2
  |> string_of_int |> print_endline
