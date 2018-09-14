let validate_parens a =
  let open Lib in
  let cs = a |> Util.explode in
  let rec aux i cs =
    match (cs, i) with
      | ([], i') -> i' = 0
      | (')' :: _, i') when i' <= 0 -> raise (Error.Invalid_expression "Unmatched opening parenthesis")
      | (')' :: tl, i') -> aux (i' - 1) tl
      | ('(' :: tl, i') -> aux (i' + 1) tl
      | (_ :: tl, _) -> aux i tl
  in
  aux 0 cs

let () =
  let a = Sys.argv.(1) in
  validate_parens a |> string_of_bool |> print_endline ;