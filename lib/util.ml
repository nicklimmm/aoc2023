(** Returns `opt` if it is `Some`, otherwise calls f and returns the result. *)
let or_else opt f = match opt with None -> f () | Some _ -> opt

let inspect f v =
  f v;
  v
