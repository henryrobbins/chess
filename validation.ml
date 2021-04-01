open Board
open Command

exception Exception

exception InvalidState

type move = square * square

type check_state =
  | Check of direction list
  | NotCheck

type pin_state =
  | Pin of direction
  | NoPin

(* TODO: FOR TESTING ONLY *)

(** [all_moves p] is all moves from piece [p]'s current square to any
    other square on the board *)
let all_moves p : move list =
  let sq = square_of_piece p in
  let rec init rows cols b =
    match rows with
    | [] -> b
    | rh :: rt -> (
        match cols with
        | [] -> init rt files b
        | ch :: ct -> init (rh :: rt) ct ((ch ^ rh) :: b))
  in
  let sq_list = init ranks files [] |> List.filter (fun x -> x <> sq) in
  List.map (fun x -> (sq, x)) sq_list

let unblocked_squares state piece direction =
  let sq = square_of_piece piece in
  let potential_squares = iterator_from_sq sq direction in
  let rec valid_moves sq_lst move_lst =
    match sq_lst with
    | [] -> List.rev move_lst
    | sq' :: t -> (
        match piece_of_square state sq' with
        | None -> valid_moves t (sq' :: move_lst)
        | Some p' ->
            if color_of_piece piece <> color_of_piece p' then
              List.rev (sq' :: move_lst)
            else List.rev move_lst)
  in
  valid_moves potential_squares []

(** [unblocked_moves state piece direction] is the list of all possible
    moves in a specific direction for piece [piece] in board state
    [state].*)
let unblocked_moves state piece direction =
  let sq = square_of_piece piece in
  let unblocked_sq = unblocked_squares state piece direction in
  List.map (fun x -> (sq, x)) unblocked_sq

(* [list_head_n lst n] is a list containing the first n elements of
   [lst] if [lst] contains more than n elements, otherwise is [lst]*)
let rec list_head_n lst n acc =
  match lst with
  | [] -> List.rev acc
  | h :: t ->
      if n = 0 then List.rev acc else list_head_n t (n - 1) (h :: acc)

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
      | Black -> [ SE; SW ])

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

let check_from_L color state =
  let king_sq = square_of_king color state in
  let check_sqs = iterator_from_sq king_sq L in
  let rec search_squares sq_lst =
    match sq_lst with
    | [] -> false
    | sq :: t -> (
        match piece_of_square state sq with
        | None -> search_squares t
        | Some piece ->
            if
              color_of_piece piece <> color
              && id_of_piece piece = Knight
            then true
            else search_squares t)
  in
  search_squares check_sqs

(** [check_from_direction c b direction] is a direction option
    indicating whether or not the player of color [c] is in check from
    direction [direction] during game state [b]. *)
let check_from_dir state dir =
  let color = color_to_move state in
  match dir with
  | L -> check_from_L color state
  | _ ->
      let king_sq = square_of_king color state in
      let king =
        match piece_of_square state king_sq with
        | Some p -> p
        | None -> failwith "King should be on the board."
      in
      let check_sqs = unblocked_squares state king dir in
      let attack_dir = invert_direction dir in
      let rec is_attacked acc sq_lst =
        match sq_lst with
        | [] -> false
        | sq :: t -> (
            match piece_of_square state sq with
            | None -> is_attacked (acc + 1) t
            | Some piece ->
                if acc > 1 then
                  match id_of_piece piece with
                  | King -> false
                  | Pawn -> false
                  | _ ->
                      List.mem attack_dir (attack_directions piece)
                      && color_of_piece piece <> color
                else
                  List.mem attack_dir (attack_directions piece)
                  && color_of_piece piece <> color)
      in
      is_attacked 1 check_sqs

(** [all_directions_attacked_from c b] is a list of all the directions
    from which the player of color [c] is checked during game state [b]. *)
let all_checks state : direction list =
  let cardinal_dirs = [ N; NE; E; SE; S; SW; W; NW; L ] in
  List.filter (fun x -> check_from_dir state x) cardinal_dirs

let is_check state : check_state =
  match all_checks state with
  | [] -> NotCheck
  | directions -> Check directions

let pawn_movement_restriction has_moved direction =
  if (not has_moved) && (direction = N || direction = S) then 2 else 1

let vert_pawn_sq piece =
  let c = color_of_piece piece in
  let sq = square_of_piece piece in
  let dir = match c with White -> N | Black -> S in
  let moved = has_moved piece in
  list_head_n
    (iterator_from_sq sq dir)
    (pawn_movement_restriction moved dir)
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

let diag_pawn_sq piece =
  let c = color_of_piece piece in
  let sq = square_of_piece piece in
  let dir = match c with White -> [ NE; NW ] | Black -> [ SE; SW ] in
  let moved = has_moved piece in
  List.map
    (fun x ->
      list_head_n (iterator_from_sq sq x)
        (pawn_movement_restriction moved x)
        [])
    dir
  |> List.flatten

(** [valid_pawn_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [P] *)
let valid_pawn_moves piece board : move list =
  let c = color_of_piece piece in
  let sq = square_of_piece piece in
  let potential_vert_sq = vert_pawn_sq piece in
  let potential_diag_sq = diag_pawn_sq piece in
  let valid_vert_sq = valid_vert_pawn_sq potential_vert_sq board in
  let enemy_piece sq' =
    match piece_of_square board sq' with
    | None -> false
    | Some p -> if color_of_piece p <> c then true else false
  in
  let valid_diag_sq = List.filter enemy_piece potential_diag_sq in
  let all_sq = valid_vert_sq @ valid_diag_sq in
  List.map (fun x -> (sq, x)) all_sq

(** [valid_rook_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [R] *)
let valid_rook_moves piece state : move list =
  let directions = [ N; S; E; W ] in
  let moves =
    List.map (fun x -> unblocked_moves state piece x) directions
  in
  List.flatten moves

(** [valid_bishop_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [B] *)
let valid_bishop_moves piece state : move list =
  let directions = [ NE; NW; SE; SW ] in
  let moves =
    List.map (fun x -> unblocked_moves state piece x) directions
  in
  List.flatten moves

(** [valid_knight_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [N] *)
let valid_knight_moves p b : move list =
  let sq = square_of_piece p in
  let valid_knight_square sq' =
    match piece_of_square b sq' with
    | None -> true
    | Some p' ->
        if color_of_piece p <> color_of_piece p' then true else false
  in
  let potential_squares = iterator_from_sq sq L in
  let valid_squares =
    List.filter valid_knight_square potential_squares
  in
  List.map (fun x -> (sq, x)) valid_squares

(** [valid_queen_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [Q] *)
let valid_queen_moves piece state : move list =
  let directions = [ N; NE; E; SE; S; SW; W; NW ] in
  let moves =
    List.map (fun x -> unblocked_moves state piece x) directions
  in
  List.flatten moves

(** [noncheck_king_move st c m] returns False if move [m] puts the king
    of color [c] in check. True otherwise. *)
let noncheck_king_move state piece move =
  let sq' = match move with _, sq -> sq in
  let state' = move_piece state piece sq' in
  match is_check state' with Check _ -> false | NotCheck -> true

(** [valid_king_moves p b cst] is the list of all valid moves for piece
    [p] with board state [b] given check state [cst]. Requires: piece
    [p] is of id [K] *)
let valid_king_moves piece state cst : move list =
  let head lst = match lst with [] -> [] | h :: t -> [ h ] in
  let directions = attack_directions piece in
  let moves =
    List.map (fun x -> head (unblocked_moves state piece x)) directions
  in
  List.flatten moves
  |> List.filter (noncheck_king_move state piece)

(** [filter_moves move_lst sq_lst] is the list of moves in [move_lst]
    where the second square of the move is in [sq_list]. *)
let filter_moves move_lst sq_lst : move list =
  let second_sq_in_list = function _, z -> List.mem z sq_lst in
  List.filter second_sq_in_list move_lst

(** [intercept_squares c b dir_lst] is the list of squares to which
    player [c] can move a piece to intercept the check on player [c]'s
    king given the king is in check from directions [dir_lst] in board
    state [b]. Requires: L is not in dir_lst*)
let intercept_squares color state dir_lst : square list =
  match piece_of_square state (square_of_king color state) with
  | None -> []
  | Some p ->
      List.map (fun x -> unblocked_squares state p x) dir_lst
      |> List.flatten

let valid_piece_moves p b cst : move list =
  let piece_type = id_of_piece p in
  match piece_type with
  | King -> valid_king_moves p b cst
  | _ -> (
      let move_lst =
        match piece_type with
        | Pawn -> valid_pawn_moves p b
        | Rook -> valid_rook_moves p b
        | Bishop -> valid_bishop_moves p b
        | Knight -> valid_knight_moves p b
        | Queen -> valid_queen_moves p b
        | _ -> []
      in
      let c = color_of_piece p in
      match cst with
      | Check dir_lst ->
          filter_moves move_lst (intercept_squares c b dir_lst)
      | NotCheck -> move_lst)

let directional_pins state color dir =
  let king_sq = square_of_king color state in
  let check_sqs = iterator_from_sq king_sq dir in
  let attack_dir = invert_direction dir in
  let rec is_attacked same_color_piece sq_lst =
    match sq_lst with
    | [] -> None
    | sq :: t -> (
        match piece_of_square state sq with
        | None -> is_attacked same_color_piece t
        | Some piece -> (
            match same_color_piece with
            | None ->
                if color_of_piece piece = color then
                  is_attacked (Some piece) t
                else None
            | Some piece' ->
                if
                  List.mem attack_dir (attack_directions piece)
                  && color_of_piece piece <> color
                then Some (piece', dir)
                else None))
  in
  is_attacked None check_sqs

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

let valid_moves b : move list =
  let c = color_to_move b in
  let cst = is_check b in
  let pieces =
    active_pieces b |> List.filter (fun x -> color_of_piece x = c)
  in
  let pin_case_router p =
    let val_moves = valid_piece_moves p b cst in
    match is_pinned p b with
    | NoPin ->
        (*print_string (string_of_string_tup_list val_moves ^ " "); *)
        val_moves
    | Pin dir ->
        List.filter (fun x -> List.mem x (pin_moves b p dir)) val_moves
  in
  List.map pin_case_router pieces |> List.flatten

let is_valid_move move b : bool =
  match move with
  | sq, sq' -> (
      match piece_of_square b sq with
      | None -> false
      | Some p ->
          let valid = valid_moves b in
          List.mem move valid)

let is_checkmate (b : Board.t) =
  match valid_moves b with
  | h :: t -> false
  | [] -> ( match is_check b with NotCheck -> false | Check _ -> true)

let is_stalemate (b : Board.t) =
  match valid_moves b with
  | h :: t -> false
  | [] -> ( match is_check b with NotCheck -> true | Check _ -> false)
