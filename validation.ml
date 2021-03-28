open Board
open Command

type move = square * square

type check_state =
  | Check of direction list
  | NotCheck

let is_check c b : check_state = NotCheck

(** [get_piece c n state] is the piece of color [c] with piece id [n]
    with game state [state] *)
let get_piece (c : color) (n : piece_type) (state : t) : p option =
  let all_pieces = active_pieces state in
  match all_pieces with
  | [] -> None
  | h :: t -> if color_of_piece (Some h) = c && id_of_piece (Some h) = n then Some h else None 

(** [intercept_squares c b dir_lst] is the list of squares to which player
    [c] can move a piece to intercept the check on player [c]'s king
    given the king is in check from directions [dir_lst] in board state
    [b]. *)
let intercept_squares c b dir_lst : square list =
  failwith "Not Implemented."

(* TODO: FOR TESTING ONLY *)

(** [all_moves p] is all moves for piece [p] *)
let all_moves p : move list =
  let sq =
    match square_of_piece (Some p) with
    | Some sq' -> sq'
    | None -> failwith "Never gonna happen..."
  in
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

(** [blocking aBoard aPiece possibleSquares] is the list of all possible
    moves in a specific direction for piece [aPiece] with board state
    [aBoard]. Requires: possibleSquares is the result of
    iterator_from_sq*)
let rec blocking aBoard aPiece possibleSquares =
  match possibleSquares with
  | h :: t ->
      if piece_of_square aBoard h != None then
        if
          color_of_piece (piece_of_square aBoard h)
          = color_of_piece (Some aPiece)
        then []
        else [ h ]
      else h :: blocking aBoard aPiece t
  | [] -> []

(** [valid_pawn_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [P] *)
let valid_pawn_moves p b : move list =
  let sq =
    match square_of_piece (Some p) with
    | Some x -> x
    | None -> failwith "The piece should be active."
  in

  let potential_moves =
    match color_of_piece (Some p) with
    | White -> iterator_from_sq sq N
    | Black -> iterator_from_sq sq S
  in

  let rec pawnblocking aBoard aPiece possibleSquares =
    match possibleSquares with
    | h :: t ->
        if piece_of_square aBoard h != None then []
        else h :: pawnblocking aBoard aPiece t
    | [] -> []
  in

  let move_up =
    if color_of_piece (Some p) = White then
      if List.mem sq (List.map (fun x -> x ^ "2") files) = true then
        match pawnblocking b p potential_moves with
        | h1 :: h2 :: t -> [ h1; h2 ]
        | [ h1 ] -> [ h1 ]
        | [] -> []
      else
        match pawnblocking b p potential_moves with
        | h1 :: t -> [ h1 ]
        | [] -> []
    else if List.mem sq (List.map (fun x -> x ^ "7") Board.files) = true
    then
      match pawnblocking b p potential_moves with
      | h1 :: h2 :: t -> [ h1; h2 ]
      | [ h1 ] -> [ h1 ]
      | [] -> []
    else
      match pawnblocking b p potential_moves with
      | h1 :: t -> [ h1 ]
      | [] -> []
  in

  let potential_attack_west =
    match color_of_piece (Some p) with
    | White -> iterator_from_sq sq NW
    | Black -> iterator_from_sq sq SW
  in
  let potential_attack_east =
    match color_of_piece (Some p) with
    | White -> iterator_from_sq sq NE
    | Black -> iterator_from_sq sq SE
  in
  let attack_west =
    match potential_attack_west with
    | h :: t ->
        if
          piece_of_square b h != None
          && color_of_piece (piece_of_square b h)
             != color_of_piece (Some p)
        then [ h ]
        else []
    | [] -> []
  in
  let attack_east =
    match potential_attack_east with
    | h :: t ->
        if
          piece_of_square b h != None
          && color_of_piece (piece_of_square b h)
             != color_of_piece (Some p)
        then [ h ]
        else []
    | [] -> []
  in
  List.map
    (fun x -> (sq, x))
    (List.append (List.append move_up attack_west) attack_east)

(** then if square_of_piece (Some p) == "a2"

    else

    match square_of_piece (Some p) with | f ^ "2" -> |

    match potential_moves with | h1 :: h2 :: t -> h1 :: h2 :: [] | h1 ::
    [] -> h1 :: [] | [] -> [] in List.map (fun x -> (sq, x)) move_list *)

(** [valid_rook_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [R] *)
let valid_rook_moves p b : move list = all_moves p

(** [valid_bishop_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [B] *)
let valid_bishop_moves p b : move list = all_moves p

(** [valid_knight_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [N] *)
let valid_knight_moves p b : move list = all_moves p

(** [valid_queen_moves p b] is the list of all valid moves (assuming no
    one is in check) for piece [p] with board state [b]. Requires: piece
    [p] is of id [Q] *)
let valid_queen_moves p b : move list = all_moves p

(** [valid_king_moves p b cst] is the list of all valid moves for piece
    [p] with board state [b] given check state [cst]. Requires: piece
    [p] is of id [K] *)
let valid_king_moves p b cst : move list = all_moves p

(** [filter_moves move_lst sq_lst] is the list of moves in [move_lst]
    where the second square of the move is in [sq_list]. *)
let filter_moves move_lst sq_lst : move list =
  let second_sq_in_list = function _, z -> List.mem z sq_lst in
  List.filter second_sq_in_list move_lst

let valid_piece_moves p b cst : move list =
  let piece_type = id_of_piece (Some p) in
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
      let c = color_of_piece (Some p) in
      match cst with
      | Check dir_lst ->
          filter_moves move_lst (intercept_squares c b dir_lst)
      | NotCheck -> move_lst)

let valid_moves c b : move list =
  let cst = is_check c b in
  let pieces =
    active_pieces b
    |> List.filter (fun x -> color_of_piece (Some x) = c)
  in
  List.map (fun p -> valid_piece_moves p b cst) pieces |> List.flatten

let is_valid_move move b : bool =
  match move with
  | sq, sq' ->
      let c = color_of_piece (piece_of_square b sq) in
      let valid = valid_moves c b in
      List.mem move valid
