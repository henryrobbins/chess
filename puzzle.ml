open Board
open Engine

type fen = string
type move = square * piece_type option

type puz = {
    current_board: fen;
    
    player_moves: int;
    computer_moves: fen list;
    wrong: bool;
}

type rush = {
    remaining: puz list;
    current_puz: puz; 
    solved: int;
    total_wrong: int;
}

let empty_puz = {
    current_board = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    
    player_moves = 0;
    computer_moves = [];
    wrong = false;
}

let get_puz_current_board puz = puz.current_board
let get_player_moves puz = puz.player_moves
let get_computer_moves puz = puz.computer_moves

let get_wrong puz = puz.wrong

let get_remaining rush = rush.remaining
let current_puz rush = rush.current_puz
let solved_rush rush = rush.solved
let wrong_rush rush = rush.total_wrong


(** [puzzle_move puz p m] is the next puzzle step in puzzle [puz], given
    that the user moved piece [p] to square [m]. If [m] was the optimal
    square to move to, [puz] advances to its next state, if there is
    one. If [puz] does not have a next state, [puzzle_step] is true. If
    [m] was not the optimal square, then [puzzle_step] is false. *)
let puzzle_move puz p m = 
    let t = init_from_fen (get_puz_current_board puz) in 

    let next_player_square = move_piece t p m true in 
    let next_player_fen = board_fen_string next_player_square in

    let best_move_square =  best_move (board_fen_string t) |> fst in
    let rank = fst best_move_square in 
    let file = snd best_move_square in 
    let best_move_piece = match board_fen_string t |> best_move |> snd with
    | Some p -> gen_piece (string_of_piece_id p) rank file 
    | None -> failwith "Impossible; no piece moved"
in
    let best_move_fen = 
        board_fen_string (move_piece t best_move_piece (rank^file) true) in 
    if next_player_fen = best_move_fen then
        let new_board =
        match get_computer_moves puz with
        | h :: t -> h
        | [] -> get_puz_current_board empty_puz
        in
        let remaining_comp_moves =
        match get_computer_moves puz with
        | h :: t -> t
        | [] -> []
        in
    {
    current_board = new_board;
    player_moves = (get_player_moves puz) - 1;
    computer_moves = remaining_comp_moves;
    wrong = false
    }
    else  {
        current_board = get_puz_current_board puz;
        player_moves = (get_player_moves puz);
        computer_moves = get_computer_moves puz;
        wrong = true
        }

let solve_puzzle rush = 
    match get_remaining (rush) with
    | h :: t -> (h, t)
    | [] -> (empty_puz, [])
        
    
let next_puz_from_rush rush puzzle_new =   
    match (get_computer_moves (puzzle_new), get_wrong puzzle_new) with
    | ([], false) -> {
        remaining = solve_puzzle rush |> snd;
        current_puz = solve_puzzle rush |> fst; 
        solved = solved_rush rush + 1;
        total_wrong = wrong_rush rush;
    }
    | (_, true) -> {
        remaining = solve_puzzle rush |> snd;
        current_puz = solve_puzzle rush |> fst; 
        solved = solved_rush rush;
        total_wrong = wrong_rush rush + 1;
    }
    | _ -> {
        remaining = get_remaining rush;
        current_puz = puzzle_new; 
        solved = solved_rush rush;
        total_wrong = wrong_rush rush;
    }

(** [play_puzzles] is the current puzzle state, given that we begin in a
    puzzle state. *)
let rec play_puzzles puz = get_puz_current_board puz |> init_from_fen 