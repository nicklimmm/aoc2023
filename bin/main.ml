let () =
  let ch = open_in "input/day1.txt" in
  Aoc2023.Day1.process ~process_words:true ch |> string_of_int |> print_endline
