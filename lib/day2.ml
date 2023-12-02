type cube_set = { red : int; green : int; blue : int }

let empty = { red = 0; green = 0; blue = 0 }

type game = { id : int; revealed : cube_set list }

let max game_a game_b =
  {
    red = max game_a.red game_b.red;
    green = max game_a.green game_b.green;
    blue = max game_a.blue game_b.blue;
  }

let game_of_string s =
  (* id *)
  let space_idx = String.index s ' ' in
  let colon_idx = String.index s ':' in
  let id =
    String.sub s (space_idx + 1) (colon_idx - space_idx - 1) |> int_of_string
  in

  (* cube_set, " 3 blue, 4 red", note the starting space *)
  let cube_set_of_string s =
    let rec aux acc = function
      | [] -> acc
      | [ _space; count; color ] :: t ->
          let count = int_of_string count in
          let new_acc =
            match color with
            | "red" -> { acc with red = acc.red + count }
            | "green" -> { acc with green = acc.green + count }
            | "blue" -> { acc with blue = acc.blue + count }
            | _ -> invalid_arg "unknown color"
          in
          aux new_acc t
      | _ -> invalid_arg "unknown pattern"
    in
    String.split_on_char ',' s
    |> List.map (String.split_on_char ' ')
    |> aux empty
  in

  (* revealed *)
  let n = String.length s in
  let rec revealed_of_string start_idx aux =
    if start_idx >= n then List.rev aux
    else
      let semi_idx =
        String.index_from_opt s start_idx ';' |> Option.value ~default:n
      in
      let cube_set =
        String.sub s start_idx (semi_idx - start_idx) |> cube_set_of_string
      in
      revealed_of_string (semi_idx + 1) (cube_set :: aux)
  in

  { id; revealed = revealed_of_string (colon_idx + 1) [] }

let is_valid game =
  List.for_all
    (fun cube_set ->
      cube_set.red <= 12 && cube_set.green <= 13 && cube_set.blue <= 14)
    game.revealed

let power_min_set game =
  let min_set = List.fold_left max empty game.revealed in
  match min_set with { red; green; blue } -> red * green * blue

let process ?(part = 1) ch =
  let rec aux ch acc =
    try
      let game = input_line ch |> game_of_string in
      match part with
      | 1 -> if is_valid game then aux ch (acc + game.id) else aux ch acc
      | 2 -> aux ch (acc + power_min_set game)
      | _ -> invalid_arg "unknown part"
    with End_of_file -> acc
  in
  aux ch 0
