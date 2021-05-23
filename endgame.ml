open Board
open Validation

type endgame =
  | K
  | K_and_N
  | K_and_B
  | Other

let is_checkmate state =
  match get_checks state with
  | NotCheck -> false
  | Check _ -> valid_moves state = []

let is_stalemate state =
  match get_checks state with
  | NotCheck -> valid_moves state = []
  | Check _ -> false

let endgame_of_piece_id_lst pid_lst =
  match pid_lst with
  | [ 'K' ] -> K
  | [ 'K'; 'N' ] | [ 'N'; 'K' ] -> K_and_N
  | [ 'K'; 'B' ] | [ 'B'; 'K' ] -> K_and_B
  | _ -> Other

let is_draw state =
  if half_turns state >= 50 then true
  else
    let strip_color str = str.[1] in
    let strip_colors (lst1, lst2) =
      (List.map strip_color lst1, List.map strip_color lst2)
    in
    let get_endgames (lst1, lst2) =
      (endgame_of_piece_id_lst lst1, endgame_of_piece_id_lst lst2)
    in
    let white_ids, black_ids =
      state |> active_pieces |> partition_pieces_by_color
      |> strip_colors |> get_endgames
    in
    match (white_ids, black_ids) with
    | Other, _ -> false
    | _, Other -> false
    | _ -> true
