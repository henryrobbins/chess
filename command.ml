open Board

type move_phrase = string list

type command =
  | Move of move_phrase
  | Quit

exception InvalidSquares

exception InconsistentPlacement

exception Malformed

let is_piece s =
  let piece_ids = [ "K"; "Q"; "R"; "B"; "N"; "P" ] in
  if List.mem s piece_ids then () else raise Malformed

(* Raises: [Malformed] if sq is not a valid square. Check this before
   is_piece_constant!*)
let is_valid_square sq =
  let ranks = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ] in
  let files = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] in
  if String.length sq = 2 then
    let fl = Char.escaped sq.[0] in
    let rk = Char.escaped sq.[1] in
    if List.mem fl files && List.mem rk ranks then ()
    else raise InvalidSquares
  else raise Malformed

let is_piece_consistent id sq board =
  match piece_of_square board sq with
  | Some p ->
      if id_of_piece p = id then () else raise InconsistentPlacement
  | None -> raise InconsistentPlacement

let is_to s = if s = "to" then () else raise Malformed

let is_valid_move_phrase lst board =
  match lst with
  | [ id; sq; w; sq' ] ->
      is_valid_square sq;
      is_valid_square sq';
      is_piece id;
      is_piece_consistent (piece_id_of_string id) sq board;
      is_to w
  | _ -> raise Malformed

let split_on_space s =
  s
  |> String.split_on_char ' '
  |> List.filter (fun x -> String.length x != 0)

let parse str board =
  match split_on_space str with
  | [] -> raise Malformed
  | h :: t -> (
      match h with
      | "move" ->
          is_valid_move_phrase t board;
          Move t
      | "quit" -> if t <> [] then raise Malformed else Quit
      | _ -> raise Malformed )
