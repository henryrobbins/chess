open Board
open Command

type move = square * square

type check_state =
  | Check of direction list
  | NotCheck

type pin_state =
  | Pin of direction
  | NoPin

let terminate_it state p' sq' move_lst =
  if color_to_move state <> color_of_piece p' then
    List.rev (sq' :: move_lst)
  else List.rev move_lst

(** [unblocked_squares state piece direction] is a list of all the
    squares in direction [direction] to which a piece [piece] in game
    state [state] can move unimpeded. *)
let unblocked_squares state sq direction =
  let potential_squares = iterator_from_sq sq direction in
  let rec valid_moves sq_lst move_lst =
    match sq_lst with
    | [] -> List.rev move_lst
    | sq' :: t -> (
        match piece_of_square state sq' with
        | None -> valid_moves t (sq' :: move_lst)
        | Some p' -> terminate_it state p' sq' move_lst )
  in
  valid_moves potential_squares []

(** [unblocked_moves state piece direction] is the list of all possible
    moves in a specific direction for piece [piece] in board state
    [state].*)
let unblocked_moves state piece direction =
  let sq = square_of_piece piece in
  let unblocked_sq = unblocked_squares state sq direction in
  List.map (fun x -> (sq, x)) unblocked_sq

(* [list_head_n lst n] is a list containing the first n elements of
   [lst] if [lst] contains more than n elements, otherwise is [lst]*)
let rec list_head_n lst n acc =
  match lst with
  | [] -> List.rev acc
  | h :: t ->
      if n = 0 then List.rev acc else list_head_n t (n - 1) (h :: acc)

(** [attack_directions piece] is a direction list indicating all the
    different direcions piece [piece] can attack.*)
let attack_directions piece =
  match id_of_piece piece with
  | King -> [ N; NE; E; SE; S; SW; W; NW ]
  | Queen -> [ N; NE; E; SE; S; SW; W; NW ]
  | Rook -> [ N; E; S; W ]
  | Bishop -> [ NE; NW; SE; SW ]
  | Knight -> [ L ]
  | Pawn -> (
      match color_of_piece piece with
      | White -> [ NE; NW ]
      | Black -> [ SE; SW ] )

let invert_direction dir =
  match dir with
  | N -> S
  | S -> N
  | E -> W
  | W -> E
  | NE -> SW
  | SW -> NE
  | NW -> SE
  | SE -> NW
  | L -> L

(** [is_attacked_from_L state sq] is true if the square [sq] is attacked
    by a knight of the color opposite the color to move in board state
    [state], and is false otherwise.*)
let is_attacked_from_L state sq =
  let check_sqs = iterator_from_sq sq L in
  let rec search_squares sq_lst =
    match sq_lst with
    | [] -> false
    | sq' :: t -> (
        match piece_of_square state sq' with
        | None -> search_squares t
        | Some piece ->
            if
              color_of_piece piece <> color_to_move state
              && id_of_piece piece = Knight
            then true
            else search_squares t )
  in
  search_squares check_sqs

let valid_directional_attack state piece dir =
  List.mem (invert_direction dir) (attack_directions piece)
  && color_of_piece piece <> color_to_move state

let rec is_attacked_from_dir state acc sq_lst dir =
  match sq_lst with
  | [] -> false
  | sq :: t -> (
      match piece_of_square state sq with
      | None -> is_attacked_from_dir state (acc + 1) t dir
      | Some piece ->
          if acc > 1 then
            match id_of_piece piece with
            | King -> false
            | Pawn -> false
            | _ -> valid_directional_attack state piece dir
          else valid_directional_attack state piece dir )

(** [check_from_dir state dir] is a boolean indicating whether or not
    the current player is in check from direction [dir] during game
    state [state]. *)
let check_from_dir state dir =
  let color = color_to_move state in
  let king_sq = square_of_king state color in
  match dir with
  | L -> is_attacked_from_L state king_sq
  | _ ->
      let check_sqs = unblocked_squares state king_sq dir in
      is_attacked_from_dir state 1 check_sqs dir

(** [all_checks state] is a (possibly empty) list of all directions from
    which the player to move is in check in game state [state]. *)
let all_checks state : direction list =
  let cardinal_dirs = [ N; NE; E; SE; S; SW; W; NW; L ] in
  List.filter (fun x -> check_from_dir state x) cardinal_dirs

(** [is_check state] is [NotCheck] if the player to move in game state
    [state] is not in check, and is [Check dir] otherwise, where [dir]
    is a list of directions from which the player is checked. *)
let get_checks state : check_state =
  match all_checks state with
  | [] -> NotCheck
  | directions -> Check directions

let has_moved_pawn p =
  let get_rank sq = Char.escaped sq.[1] in
  let sq = square_of_piece p in
  match color_of_piece p with
  | White -> get_rank sq <> "2"
  | Black -> get_rank sq <> "7"

let pawn_movement_restriction p direction =
  if (not (has_moved_pawn p)) && (direction = N || direction = S) then 2
  else 1

let vert_pawn_sq p =
  let c = color_of_piece p in
  let sq = square_of_piece p in
  let dir = match c with White -> N | Black -> S in
  list_head_n
    (iterator_from_sq sq dir)
    (pawn_movement_restriction p dir)
    []

let valid_vert_pawn_sq sq_lst board =
  let is_empty sq =
    match piece_of_square board sq with None -> true | Some _ -> false
  in
  match sq_lst with
  | [] -> []
  | [ sq' ] -> if is_empty sq' then [ sq' ] else []
  | [ sq'; sq'' ] ->
      if is_empty sq' && is_empty sq'' then [ sq'; sq'' ]
      else if is_empty sq' then [ sq' ]
      else []
  | _ -> []

let diag_pawn_sq p =
  let c = color_of_piece p in
  let sq = square_of_piece p in
  let dir = match c with White -> [ NE; NW ] | Black -> [ SE; SW ] in
  List.map
    (fun x ->
      list_head_n (iterator_from_sq sq x)
        (pawn_movement_restriction p x)
        [])
    dir
  |> List.flatten

(** [valid_pawn_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [Pawn] *)
let valid_pawn_moves state piece : move list =
  let sq = square_of_piece piece in
  let vert_sqs = vert_pawn_sq piece in
  let diag_sqs = diag_pawn_sq piece in
  let valid_vert_sq = valid_vert_pawn_sq vert_sqs state in
  let is_enemy_piece sq' =
    match piece_of_square state sq' with
    | None -> Some sq' = en_passant_sq state
    | Some p -> color_of_piece p <> color_to_move state
  in
  let valid_diag_sq = List.filter is_enemy_piece diag_sqs in
  let all_sq = valid_vert_sq @ valid_diag_sq in
  List.map (fun x -> (sq, x)) all_sq

(** [valid_rook_moves state piece] is the list of all valid moves
    (assuming no one is in check) for [piece] in game state [state].
    Requires: [piece] has id [Rook] *)
let valid_rook_moves state piece : move list =
  let directions = [ N; S; E; W ] in
  let moves =
    List.map (fun x -> unblocked_moves state piece x) directions
  in
  List.flatten moves

(** [valid_bishop_moves state piece] is the list of all valid moves
    (assuming no one is in check) for [piece] in game state [state].
    Requires: [piece] has id [Bishop] *)
let valid_bishop_moves state piece : move list =
  let directions = [ NE; NW; SE; SW ] in
  let moves =
    List.map (fun x -> unblocked_moves state piece x) directions
  in
  List.flatten moves

let valid_knight_sq p b sq =
  match piece_of_square b sq with
  | None -> true
  | Some p' -> color_of_piece p <> color_of_piece p'

(** [valid_knight_moves state piece] is the list of all valid moves
    (assuming no one is in check) for [piece] in game state [state].
    Requires: [piece] has id [Knight] *)
let valid_knight_moves state piece : move list =
  let sq = square_of_piece piece in
  let potential_squares = iterator_from_sq sq L in
  let valid_squares =
    List.filter (valid_knight_sq piece state) potential_squares
  in
  List.map (fun x -> (sq, x)) valid_squares

(** [valid_queen_moves state piece] is the list of all valid moves
    (assuming no one is in check) for [piece] in game state [state].
    Requires: [piece] has id [Queen] *)
let valid_queen_moves state piece : move list =
  let directions = [ N; NE; E; SE; S; SW; W; NW ] in
  let moves =
    List.map (fun x -> unblocked_moves state piece x) directions
  in
  List.flatten moves

(** [noncheck_king_move st c m] returns False if move [m] puts the king
    of color [c] in check. True otherwise. *)
let noncheck_king_move state piece move =
  let sq' = match move with _, sq -> sq in
  let state' = move_piece state piece sq' false in
  match get_checks state' with Check _ -> false | NotCheck -> true

(** [valid_king_moves p b] is the list of all valid moves for piece [p]
    with board state [b]. Requires: piece [p] is of id [K] *)
let valid_king_moves p b : move list =
  let head lst = match lst with [] -> [] | h :: t -> [ h ] in
  let directions = attack_directions p in
  let moves =
    List.flatten
      (List.map (fun x -> head (unblocked_moves b p x)) directions)
  in
  List.filter (noncheck_king_move b p) moves

(** [filter_moves move_lst sq_lst] is the list of moves in [move_lst]
    where the second square of the move is in [sq_list]. *)
let filter_moves move_lst sq_lst : move list =
  let second_sq_in_list = function _, z -> List.mem z sq_lst in
  List.filter second_sq_in_list move_lst

let find_L_check_sq state =
  let king_sq = square_of_king state (color_to_move state) in
  let rec find_aux sq_lst =
    match sq_lst with
    | [] -> failwith "impossible" [@coverage off]
    | sq :: t -> (
        match piece_of_square state sq with
        | None -> find_aux t
        | Some p ->
            if
              id_of_piece p = Knight
              && color_to_move state <> color_of_piece p
            then [ sq ]
            else find_aux t )
  in
  find_aux (iterator_from_sq king_sq L)

let intercept_router state dir =
  match dir with
  | L -> find_L_check_sq state
  | _ ->
      let king_sq = square_of_king state (color_to_move state) in
      unblocked_squares state king_sq dir

(** [intercept_squares c b dir_lst] is the list of squares to which
    player [c] can move a piece to intercept the check on player [c]'s
    king given the king is in check from directions [dir_lst] in board
    state [b]. Requires: L is not in dir_lst*)
let intercept_squares state dir_lst : square list =
  if List.length (all_checks state) > 1 then []
  else
    List.map (fun x -> intercept_router state x) dir_lst |> List.flatten

let extract_sq_option sq =
  match sq with
  | None -> failwith "Invalid Application" [@coverage off]
  | Some sq' -> sq'

let castle_empty_spaces state side_id =
  let color = color_to_move state in
  let king_square = square_of_king state color in
  match side_id with
  | King -> List.length (unblocked_squares state king_square E) = 2
  | Queen -> List.length (unblocked_squares state king_square W) = 3
  | _ -> failwith "impossible" [@coverage off]

(*let rec is_attacked_from_dir acc sq_lst state dir =*)

let sq_not_attacked state dir_list sq =
  let rec attack_checker dir_list' =
    match dir_list' with
    | [] -> true
    | h :: t -> (
        match h with
        | L ->
            if is_attacked_from_L state sq then false
            else attack_checker t
        | _ ->
            let squares = unblocked_squares state sq h in
            if is_attacked_from_dir state 1 squares h then false
            else attack_checker t )
  in
  attack_checker dir_list

let cast_atk_dirs state =
  match color_to_move state with
  | White -> [ N; NE; NW; L ]
  | Black -> [ S; SE; SW; L ]

let castle_checked_spaces b side_id =
  let color = color_to_move b in
  let king_square = square_of_king b color in
  let space_list =
    match side_id with
    | King -> unblocked_squares b king_square E
    | Queen -> (
        match unblocked_squares b king_square W with
        | [ h1; h2; h3 ] -> [ h1; h2 ]
        | _ -> failwith "impossible" [@coverage off])
    | _ -> failwith "impossible" [@coverage off]
  in
  let dir_list = cast_atk_dirs b in
  List.length (List.filter (sq_not_attacked b dir_list) space_list) = 2

let castle_valid b side_id =
  let color = color_to_move b in
  if castle_empty_spaces b side_id then
    castle_checked_spaces b side_id
    && get_checks b = NotCheck
    && can_castle b color side_id
  else false

let castle_moves b =
  let color = color_to_move b in
  let ks_move =
    if castle_valid b King then
      match color with
      | White -> [ ("e1", "g1") ]
      | Black -> [ ("e8", "g8") ]
    else []
  in
  let qs_move =
    if castle_valid b Queen then
      match color with
      | White -> [ ("e1", "c1") ]
      | Black -> [ ("e8", "c8") ]
    else []
  in
  ks_move @ qs_move

let valid_move_router state piece =
  match id_of_piece piece with
  | Pawn -> valid_pawn_moves state piece
  | Rook -> valid_rook_moves state piece
  | Bishop -> valid_bishop_moves state piece
  | Knight -> valid_knight_moves state piece
  | Queen -> valid_queen_moves state piece
  | _ -> []

let include_ep_intercepts state piece dir_lst =
  let intercepts = intercept_squares state dir_lst in
  match en_passant_piece state with
  | None -> intercepts
  | Some p ->
      if
        id_of_piece piece = Pawn
        && List.mem (square_of_piece p) intercepts
      then (en_passant_sq state |> extract_sq_option) :: intercepts
      else intercepts

let piece_moves_without_pins state piece : move list =
  let cst = get_checks state in
  let piece_type = id_of_piece piece in
  match piece_type with
  | King -> valid_king_moves piece state @ castle_moves state
  | _ -> (
      let move_lst = valid_move_router state piece in
      match cst with
      | NotCheck -> move_lst
      | Check dir_lst ->
          let intercepts = include_ep_intercepts state piece dir_lst in
          filter_moves move_lst intercepts )

let rec get_attacks same_color_piece sq_lst state color dir =
  match sq_lst with
  | [] -> None
  | sq :: t -> (
      match piece_of_square state sq with
      | None -> get_attacks same_color_piece t state color dir
      | Some piece -> (
          match same_color_piece with
          | None ->
              if color_of_piece piece = color then
                get_attacks (Some piece) t state color dir
              else None
          | Some piece' -> (
              match id_of_piece piece with
              | King | Pawn -> None
              | _ ->
                  if valid_directional_attack state piece dir then
                    Some (piece', dir)
                  else None ) ) )

let directional_pins state color dir =
  let king_sq = square_of_king state color in
  let check_sqs = iterator_from_sq king_sq dir in
  get_attacks None check_sqs state color dir

let pinned_pieces state color : (p * direction) list =
  [ N; NE; E; SE; S; SW; W; NW ]
  |> List.map (directional_pins state color)
  |> List.filter (fun x ->
         match x with None -> false | Some _ -> true)
  |> List.map (fun x ->
         match x with None -> failwith "filtered out" | Some x' -> x')

let is_pinned piece state =
  try
    Pin (List.assoc piece (pinned_pieces state (color_of_piece piece)))
  with Not_found -> NoPin

let pin_moves state piece dir =
  let rev_dir = invert_direction dir in
  [
    unblocked_moves state piece dir; unblocked_moves state piece rev_dir;
  ]
  |> List.flatten

let valid_piece_moves state piece : move list =
  let moves = piece_moves_without_pins state piece in
  match is_pinned piece state with
  | NoPin -> moves
  | Pin dir ->
      List.filter
        (fun x -> List.mem x (pin_moves state piece dir))
        moves

let valid_moves b : move list =
  let c = color_to_move b in
  let pieces =
    active_pieces b |> List.filter (fun x -> color_of_piece x = c)
  in
  List.map (valid_piece_moves b) pieces |> List.flatten

let is_valid_move move b : bool =
  match move with
  | sq, sq' -> (
      match piece_of_square b sq with
      | None -> false
      | Some p ->
          let valid = valid_moves b in
          List.mem move valid )
