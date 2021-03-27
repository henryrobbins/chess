open Board
open Command

type move = (square * square)

type check_state =
    | Check of direction list
    | NotCheck

let is_check c b : check_state = failwith("Not Implemented.")

(** [intercept_squares c b dir_lst] is the list of squares which player [c] can
    move a piece to intercept the check on player [c]'s king given the king is
    in check from directions [dir_lst] in board state [b]. *)
let intercept_squares c b dir_lst : square list = failwith("Not Implemented.")

(** [valid_pawn_moves p b] is the list of all valid moves (assuming no one is
    in check) for piece [p] with board state [b].
    Requires: piece [p] is of id [P] *)
let valid_pawn_moves p b : move list = failwith("Not Implemented.")

(** [valid_rook_moves p b] is the list of all valid moves (assuming no one is
    in check) for piece [p] with board state [b].
    Requires: piece [p] is of id [R] *)
let valid_rook_moves p b : move list = failwith("Not Implemented.")

(** [valid_bishop_moves p b] is the list of all valid moves (assuming no one is
    in check) for piece [p] with board state [b].
    Requires: piece [p] is of id [B] *)
let valid_bishop_moves p b : move list = failwith("Not Implemented.")

(** [valid_knight_moves p b] is the list of all valid moves (assuming no one is
    in check) for piece [p] with board state [b].
    Requires: piece [p] is of id [N] *)
let valid_knight_moves p b : move list = failwith("Not Implemented.")

(** [valid_queen_moves p b] is the list of all valid moves (assuming no one is
    in check) for piece [p] with board state [b].
    Requires: piece [p] is of id [Q] *)
let valid_queen_moves p b : move list = failwith("Not Implemented.")

(** [valid_king_moves p b cst] is the list of all valid moves for piece [p]
    with board state [b] given check state [cst].
    Requires: piece [p] is of id [K] *)
let valid_king_moves p b cst : move list = failwith("Not Implemented.")


(** [filter_moves move_lst sq_lst] is the list of moves in [move_lst] where
    the second square of the move is in [sq_list]. *)
let filter_moves move_lst sq_lst : move list =
    let second_sq_in_list = function (_,z) -> List.mem z sq_lst in
    List.filter second_sq_in_list move_lst

let valid_piece_moves p b cst : move list =
    let piece_type = id_of_piece (Some p) in
    match piece_type with
    | King -> valid_king_moves p b cst
    | _ ->
        let move_lst =
            match piece_type with
            | Pawn -> valid_pawn_moves p b
            | Rook -> valid_rook_moves p b
            | Bishop -> valid_bishop_moves p b
            | Knight -> valid_knight_moves p b
            | Queen -> valid_queen_moves p b
            | _ -> [] in
        let c = color_of_piece (Some p) in
           match cst with
           | Check dir_lst -> filter_moves move_lst (intercept_squares c b dir_lst)
           | NotCheck -> move_lst

let valid_moves c b : move list =
    let cst = is_check c b in
    let pieces = active_pieces b |> List.filter (fun x -> color_of_piece (Some x) = c) in
    List.map (fun p -> valid_piece_moves p b cst) pieces |> List.flatten
