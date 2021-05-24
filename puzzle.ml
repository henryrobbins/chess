open Board
open Engine
open Random
open Yojson.Basic.Util

type fen = string

let string_of_string_list lst =
  let rev_lst = List.rev lst in
  let rec build_str str lst' =
    match lst' with
    | [] -> str ^ "]"
    | h :: t -> build_str (str ^ h ^ ", ") t
  in
  build_str "[" rev_lst

type progress =
  | InProgress
  | Complete
  | GameOver
  | Correct
  | Wrong

type puzzle = {
  description : string;
  computer_color : color;
  board : fen ref;
  player_moves : fen list ref;
  computer_moves : fen list ref;
}

type rush = {
  puzzles : puzzle list ref;
  current_puz : puzzle ref;
  total_solved : int ref;
  total_wrong : int ref;
}

let extract_puzzle j =
  let description = j |> member "description" |> to_string in
  let init_board = j |> member "current_board" |> to_string in
  let computer_color =
    match color_to_move (init_from_fen init_board) with
    | Black -> White
    | White -> Black
  in
  let player_moves =
    j |> member "player_moves" |> to_list |> List.map to_string
  in
  let computer_moves =
    j |> member "computer_moves" |> to_list |> List.map to_string
  in
  {
    description;
    computer_color;
    board = ref init_board;
    player_moves = ref player_moves;
    computer_moves = ref computer_moves;
  }

let puzzles () =
  "puzzles.json" |> Yojson.Basic.from_file |> to_list
  |> List.map extract_puzzle

let computer_color rush = !(rush.current_puz).computer_color

let total_solved rush = !(rush.total_solved)

let total_wrong rush = !(rush.total_wrong)

let current_board rush = init_from_fen !(!(rush.current_puz).board)

let update_puzzle rush =
  match !(rush.puzzles) with
  | [] -> false
  | h :: t ->
      rush.current_puz := h;
      rush.puzzles := t;
      true

let update_wrong rush =
  let total_wrong = !(rush.total_wrong) + 1 in
  rush.total_wrong := total_wrong;
  if total_wrong = 3 then GameOver
  else if update_puzzle rush then Wrong
  else Complete

let correct_end_puzzle_return rush =
  if update_puzzle rush then Correct else Complete

let update_rush_with_computer_move rush player_moves' =
  !(rush.current_puz).player_moves := player_moves';
  ( match !(!(rush.current_puz).computer_moves) with
  | [] -> failwith "impossible"
  | computer_move :: computer_moves' ->
      !(rush.current_puz).board := computer_move;
      !(rush.current_puz).computer_moves := computer_moves' );
  ()

let update_rush_with_move rush fen =
  let player_moves = !(!(rush.current_puz).player_moves) in
  match player_moves with
  | [] -> failwith "impossible 2"
  | [ correct_fen ] ->
      if fen = correct_fen then (
        !(rush.current_puz).board := fen;
        !(rush.current_puz).player_moves := [];
        rush.total_solved := !(rush.total_solved) + 1;
        correct_end_puzzle_return rush )
      else update_wrong rush
  | correct_fen :: player_moves' ->
      if fen = correct_fen then (
        update_rush_with_computer_move rush player_moves';
        InProgress )
      else update_wrong rush

(** [random_subset lb ub n] returns [n] random integers between lower
    bound [lb] (inclusive) and upper bound [ub] (exclusive). *)
let random_subset lb ub n =
  let rec set_aux acc i =
    if i = ub then acc else set_aux (i :: acc) (i + 1)
  in
  let set = set_aux [] lb in
  let rec random_subset_aux subset draw_set =
    if List.length subset = n then subset
    else
      let i = Random.int (List.length draw_set) in
      let elt = List.nth draw_set i in
      let draw_set' = List.filter (fun x -> x <> elt) draw_set in
      random_subset_aux (elt :: subset) draw_set'
  in
  random_subset_aux [] set

(** [get_puzzles indices] returns the puzzles at the given [indices] *)
let get_puzzles indices =
  List.map (fun x -> List.nth (puzzles ()) x) indices

let init_rush () =
  let puzzle_list =
    get_puzzles (random_subset 0 15 4)
    @ get_puzzles (random_subset 15 30 4)
    @ get_puzzles (random_subset 30 45 2)
    @ get_puzzles (random_subset 45 60 2)
    @ get_puzzles (random_subset 45 60 2)
    @ get_puzzles (random_subset 60 70 2)
    @ get_puzzles (random_subset 70 75 2)
    @ get_puzzles (random_subset 75 80 2)
    @ get_puzzles (random_subset 80 85 2)
  in
  {
    puzzles = ref (List.tl puzzle_list);
    current_puz = ref (List.hd puzzle_list);
    total_solved = ref 0;
    total_wrong = ref 0;
  }
