open Board
open Command

exception Exception
exception InvalidPieceType

type move = square * square

type check_state =
  | Check of direction list
  | NotCheck

(* TODO: FOR TESTING ONLY *)

(** [all_moves p] is all moves for piece [p] *)
let all_moves p : move list =
  let sq = square_of_piece p in
  let rec init rows cols b =
    match rows with
    | [] -> b
    | rh :: rt -> (
        match cols with
        | [] -> init rt files b
        | ch :: ct -> init (rh :: rt) ct ((ch ^ rh) :: b) )
  in
  let sq_list = init ranks files [] |> List.filter (fun x -> x <> sq) in
  List.map (fun x -> (sq, x)) sq_list

let unblocked_squares state piece direction =
  let sq = square_of_piece piece in
  let potential_squares = iterator_from_sq sq direction in
  let rec valid_moves sq_lst move_lst =
    match sq_lst with
    | [] -> move_lst
    | sq' :: t -> (
        match piece_of_square state sq' with
        | None -> valid_moves t (sq' :: move_lst)
        | Some p' ->
            if color_of_piece piece <> color_of_piece p' then
              sq' :: move_lst
            else move_lst )
  in
  valid_moves potential_squares []

(** [unblocked_moves state piece direction] is the list of all possible
    moves in a specific direction for piece [aPiece] with board state
    [aBoard]. Requires: possibleSquares is the result of
    iterator_from_sq *)
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

(** [valid_king_moves p b cst] is the list of all valid moves for piece
    [p] with board state [b] given check state [cst]. Requires: piece
    [p] is of id [K] *)
let valid_king_moves piece state cst : move list =
  let prohib_directions =
    match cst with Check dir_lst -> dir_lst | NotCheck -> []
  in
  let check_direction dir =
    if List.mem dir prohib_directions then false else true
  in
  let head lst = match lst with [] -> [] | h :: t -> [ h ] in
  let directions =
    List.filter check_direction [ N; NE; E; SE; S; SW; W; NW ]
  in
  let moves =
    List.map (fun x -> head (unblocked_moves state piece x)) directions
  in
  List.flatten moves

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
      | NotCheck -> move_lst )
    
(** [check_from_direction c b direction] is a direction option
indicating whether or not the player of color [c] is in check from
direction [direction] during game state [b]. *)
let check_from_direction (c : color) (b : t) (direction : direction) : direction option =
  let king_sq = square_of_king c b in
  match active_pieces b with
  | [] -> None
  | h :: t ->
    let h_current_square = square_of_piece h in
    let poss_moves = iterator_from_sq h_current_square direction in
    let contained = List.mem king_sq poss_moves in
    if contained then Some direction else None

(** [all_directions_attacked_from c b] is a list of all the directions
from which the player of color [c] is checked during game state [b]. *)
let all_directions_attacked_from (c : color) (b : t) : direction list =
  let directions = [ N; NE; E; SE; S; SW; W; NW; L ] in
  let rec attacked_from
    (l : direction list)
    (final_list : direction list) =
  match l with
  | [] -> []
  | h :: t -> begin
    let direc = check_from_direction c b h in
    match direc with
    | None -> []
    | Some d -> d :: attacked_from t final_list 
  end in attacked_from directions []

(** [get_piece c n state] is the piece of color [c] with piece id [n]
in game state [state] *)
let get_piece (c : color) (n : piece_type) (state : t) : p =
  let all_pieces = active_pieces state in
  match all_pieces with
  | [] -> raise InvalidPieceType
  | h :: t -> if color_of_piece h = c && id_of_piece h = n then h else raise InvalidPieceType

(** [piece_check ing_square valid_moves] is a boolean value representing whether or not [king_square] 
    is contained in a list containing another piece's [valid_moves]*)
let rec piece_check (king_square : square) (valid_moves : (string * string) list) : bool = 
  match valid_moves with 
  | [] -> false
  | h :: t -> begin
    match h with 
    | (_, y) -> if y = king_square then true else piece_check king_square t  
  end

(** [check_from_direction c b direction] is a direction option indicating whether or not the player
of color [c] is in check from direction [direction] during game state [b]. *)
let check_from_direction (c : color) (b : t) (direction : direction) : direction option = 
  let king = get_piece c King b in
  let king_square = square_of_piece king in
  let potential_pieces = iterator_from_sq king_square direction in
  (* Find all the pieces in a given direction*)
  let rec find_all_pieces square_list = 
  match square_list with 
  | [] -> None
  | h :: t -> begin
    let h_piece = piece_of_square b h in
    match h_piece with 
    | None -> find_all_pieces t 
    | Some piece -> begin
      let valid_moves = valid_piece_moves piece b NotCheck in
      let checked = piece_check king_square valid_moves in
      if checked = true then Some direction else find_all_pieces t
    end 
    end  
  in find_all_pieces potential_pieces

(** [all_directions_attacked_from c b] is a list of all the directions from which the player
    of color [c] is checked during game state [b]. *)
let all_directions_attacked_from (c : color) (b : t) : direction list = 
  let directions = [N; NE; E; SE; S; SW; W; NW; L] in 
  let rec attacked_from (l : direction list) (final_list : direction list) = 
    match l with 
    | [] -> []
    | h :: t -> begin
      let direc = check_from_direction c b h in 
      match direc with 
      | None -> []
      | Some d -> d :: attacked_from t final_list
    end 
  in attacked_from directions []

let is_check (c : color) (b : t) : check_state = 
  let dirs = all_directions_attacked_from c b in 
  match dirs with 
  | [] -> NotCheck
  | h :: t -> Check dirs 

let valid_moves c b : move list =
  let cst = is_check c b in
  let pieces =
    active_pieces b |> List.filter (fun x -> color_of_piece x = c)
  in
  List.map (fun p -> valid_piece_moves p b cst) pieces |> List.flatten

let is_valid_move move b : bool =
  match move with
  | sq, sq' -> (
      match piece_of_square b sq with
      | None -> false
      | Some p ->
          let c = color_of_piece p in
          let valid = valid_moves c b in
          List.mem move valid )

