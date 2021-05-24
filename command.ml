open Board

type move_phrase = string list

type command =
  | Move of move_phrase
  | Quit

exception InvalidSquares

exception InconsistentPlacement

exception Malformed

(** [is_piece s] returns a unit if string [s] represents a piece type.

    Raises: [Malformed] if the string [s] does not represent a piece
    type. *)
let is_piece s =
  let piece_ids = [ "K"; "Q"; "R"; "B"; "N"; "P" ] in
  if List.mem s piece_ids then () else raise Malformed

(** [is_valid_square sq] returns unit if string [s] is a valid board
    square.

    Raises: [Malformed] if the string is not of length 2.

    Raises: [InvalidSquares] if either the first or second char of [s]
    are not a valid file or rank respectively. *)
let is_valid_square sq =
  let ranks = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ] in
  let files = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] in
  if String.length sq = 2 then
    let fl = Char.escaped sq.[0] in
    let rk = Char.escaped sq.[1] in
    if List.mem fl files && List.mem rk ranks then ()
    else raise InvalidSquares
  else raise Malformed

(** [is_piece_consistent id sq b] returns a unit if the piece type [id]
    matches the piece type of the piece on square [sq] in state [b].

    Raises: [InconsistentPlacement] the piece type [id] does not match
    the piece type of the piece on square [sq] in state [b]. *)
let is_piece_consistent id sq board =
  match piece_of_square board sq with
  | Some p ->
      if id_of_piece p = id then () else raise InconsistentPlacement
  | None -> raise InconsistentPlacement

(** [is_to s] returns a unit if string [s] is "to".

    Raises: [Malformed] if string [s] is not "to". *)
let is_to s = if s = "to" then () else raise Malformed

(** [is_valid_move_phrase lst b] returns unit if the move phrase [lst]
    is valid.

    Raises: [Malformed] if the list [lst] has more than four items. *)
let is_valid_move_phrase lst board =
  match lst with
  | [ id; sq; w; sq' ] ->
      is_valid_square sq;
      is_valid_square sq';
      is_piece id;
      is_piece_consistent (piece_id_of_string id) sq board;
      is_to w
  | _ -> raise Malformed

(** [split_on_space s] is the string [s] split on space where multiple
    spaces are considered as a single space. *)
let split_on_space s =
  s
  |> String.split_on_char ' '
  |> List.filter (fun x -> String.length x != 0)

let quit_phrase t = if t <> [] then raise Malformed else Quit

let parse str board =
  match split_on_space str with
  | [] -> raise Malformed
  | h :: t -> (
      match h with
      | "move" ->
          is_valid_move_phrase t board;
          Move t
      | "quit" -> quit_phrase t
      | _ -> raise Malformed )
