(************************************************************

   Test Plan

   We deemed the following modules to require automatic testing through OUnit
   testing: Board, Command, Endgame, Engine, and Validation. Coverage on a
   small fraction of these modules was ignored due to one of:

   1 ) The code was unreachable (failwith "impossible").
   2 ) Code which only printed output to the terminal.

   In the case of (2), this code was manually tested. Our OUnit test were
   developed through both black box and glass box testing. We did not use any
   randomized testing. Our initial pass on tests was based on black box
   testing. However, we deemed that a second pass using glass box testing was
   necessary since a chess implementation requires lots of conditional flow.
   Through glass box testing and the coverage tool bisect, we could verify
   that our code had the expected output in all possible branches.

   The modules that were not automatically tested include Game_text, Game_gui,
   and Puzzle. These three modules were manually tested. In the case of the
   first two, which maintained the command-line and GUI versions of our
   chess game respectively, it is common practice to test this manually as it
   is often infeasible to test graphical output. In the case of the third,
   we only have 85 puzzles and we manually tested each one through the GUI.

   The correctness of this system is accurately measured through this testing
   plan as our automatic tests have over 80% coverage. In addition, the GUI
   and command-line game have been extensivley tested by the four code
   contributors in additon to our project manager. Furthermore, these 5
   individuals cover OXS, Windows, and Linux machines on which the system
   preformed successfully.

 ************************************************************)

open OUnit2
open Board
open Endgame
open Command
open Validation
open Puzzle
open Yojson.Basic.Util
open Engine

let board = init_game ()

let direction_from_string str =
  match str with
  | "N" -> N
  | "NE" -> NE
  | "E" -> E
  | "SE" -> SE
  | "S" -> S
  | "SW" -> SW
  | "W" -> W
  | "NW" -> NW
  | "L" -> L
  | _ -> failwith "invalid direction"

type test_instance = {
  fen : string;
  check : check_state;
  moves : move list;
  draw : bool;
}

let extract j =
  let description = j |> member "description" |> to_string in
  let fen = j |> member "fen" |> to_string in
  let check =
    let check_list =
      j |> member "check" |> to_list |> List.map to_string
      |> List.map direction_from_string
    in
    match check_list with [] -> NotCheck | l -> Check l
  in
  let lst_to_tuple lst =
    match lst with [ x; y ] -> (x, y) | _ -> failwith "not a tuple"
  in
  let parse_move x =
    x |> to_list |> List.map to_string |> lst_to_tuple
  in
  let moves = j |> member "moves" |> to_list |> List.map parse_move in
  let draw = j |> member "draw" |> to_bool in
  (description, { fen; check; moves; draw })

let init_tests_json json extraction_fn =
  json |> Yojson.Basic.from_file |> to_list |> List.map extraction_fn

let tests = init_tests_json "test_fens.json" extract

type puzzle_instance = {
  current_board : string;
  player_moves : string list;
  computer_moves : string list;
  wrong : bool;
  complete : bool;
}

let extract_puz j =
  let description = j |> member "description" |> to_string in
  let current_board = j |> member "current_board" |> to_string in
  let player_moves =
    j |> member "player_moves" |> to_list |> List.map to_string
  in
  let computer_moves =
    j |> member "computer_moves" |> to_list |> List.map to_string
  in
  let wrong = j |> member "wrong" |> to_string |> bool_of_string in
  let complete =
    j |> member "complete" |> to_string |> bool_of_string
  in
  ( description,
    { current_board; player_moves; computer_moves; wrong; complete } )

let puzzle_tests = init_tests_json "puzzles.json" extract_puz

(* HELPER FUNCTIONS *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let string_of_string_tup_list tup_lst =
  let rev_lst = List.rev tup_lst in
  let rec build_str str lst' =
    match lst' with
    | [] -> str
    | (h, h') :: t ->
        build_str (str ^ "(" ^ h ^ ", " ^ h' ^ ")" ^ ", ") t
  in
  build_str "" rev_lst

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let print_tuple = function a, b -> "(" ^ a ^ ", " ^ b ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [pp_pair pp1 pp2 (a, b)] pretty-prints [(a, b)] using [pp1] for [a]
    and [pp2] for [b]. *)
let pp_pair pp1 pp2 (a, b) = "(" ^ pp1 a ^ ", " ^ pp2 b ^ ")"

let pp_dir dir =
  match dir with
  | N -> "N"
  | S -> "S"
  | SW -> "SW"
  | SE -> "SE"
  | NW -> "NW"
  | NE -> "NE"
  | W -> "W"
  | E -> "E"
  | L -> "L"

let pp_dirs dirs =
  let rec string_maker dirs acc =
    match dirs with
    | [ h ] -> acc ^ pp_dir h ^ "]"
    | h :: t -> acc ^ pp_dir h ^ ";" ^ string_maker t acc
    | [] -> acc ^ "]"
  in
  "[" ^ string_maker dirs ""

let extract_piece piece_option =
  match piece_option with None -> failwith "no piece" | Some p -> p

let move b sq sq' =
  let p = piece_of_square b sq |> extract_piece in
  move_piece b p sq' true

(** [move_piece_test name b s s'] constructs OUnit tests named [name]
    that assert [move_piece b (piece_of_square s) s'] is correct. *)
let move_piece_test name b s s' : test list =
  let p = piece_of_square b s |> extract_piece in
  let b' = move_piece b p s' true in
  let p' = piece_of_square b' s' |> extract_piece in
  [
    ( name ^ " | previous board square" >:: fun _ ->
      assert_equal None (piece_of_square b' s) );
    ( name ^ " | new board square (piece id) " >:: fun _ ->
      assert_equal (id_of_piece p) (id_of_piece p') );
    ( name ^ " | new board square (piece color) " >:: fun _ ->
      assert_equal (color_of_piece p) (color_of_piece p') );
    ( name ^ " | new board square (piece pos) " >:: fun _ ->
      assert_equal (square_of_piece p') s' ~printer:(fun x -> x) );
  ]

(** [move_piece_special_moves_test name b p s expected_ep expected_cast]
    constructs a list of OUnit tests named [name] that assert the
    equality of [expected_ep] with the en-passant state in game state
    [b] and the boolean tuple [expected_cast] with the castle states in
    game state [b].*)
let move_piece_special_moves_test name b s s' expected_ep expected_cast
    =
  let b' = move b s s' in
  match expected_cast with
  | wk, wq, bk, bq ->
      [
        ( name ^ " | en_passant" >:: fun _ ->
          assert_equal expected_ep (en_passant_sq b') );
        ( name ^ " | white_kingside_castle" >:: fun _ ->
          assert_equal wk (can_castle b' White King) );
        ( name ^ " | white_queenside_castle" >:: fun _ ->
          assert_equal wq (can_castle b' White Queen) );
        ( name ^ " | black_kingside_castle" >:: fun _ ->
          assert_equal bk (can_castle b' Black King) );
        ( name ^ " | black_queenside_castle" >:: fun _ ->
          assert_equal bq (can_castle b' Black Queen) );
      ]

(** [iterator_from_sq_test name board s d expected] constructs an OUnit
    test named [name] that asserts the equality of [expected] with
    [iterator_from_sq s d]. *)
let iterator_from_sq_test name s d expected : test =
  name >:: fun _ ->
  assert_equal expected (iterator_from_sq s d) ~printer:(pp_list Fun.id)

let value_captured_test name color exp =
  name >:: fun _ ->
  let test = List.assoc name tests in
  let board = init_from_fen test.fen in
  assert_equal exp (value_of_captured board color)

let board_tests =
  [
    (* move_pieces tests *)
    (* TODO: add test cases for captured pieces. *)
    move_piece_test "d2 -> d4" board "d2" "d4";
    move_piece_test "d2 -> d3" board "d2" "d3";
    move_piece_test "b1 -> c3" board "b1" "c3";
    move_piece_test "g1 -> h3" board "g1" "h3";
    (* move_piece special moves tests *)
    move_piece_special_moves_test "d2 -> d4, d3 becomes en-passant"
      board "d2" "d4" (Some "d3")
      (true, true, true, true);
    (let board' = move board "e2" "e4" in
     let board'' = move board' "e7" "e6" in
     move_piece_special_moves_test "bong cloud can't castle" board''
       "e1" "e2" None
       (false, false, true, true));
    (* iterator_from_sq tests *)
    [
      iterator_from_sq_test "d5 -> N" "d5" N [ "d6"; "d7"; "d8" ];
      iterator_from_sq_test "d5 -> NE" "d5" NE [ "e6"; "f7"; "g8" ];
      iterator_from_sq_test "d5 -> E" "d5" E [ "e5"; "f5"; "g5"; "h5" ];
      iterator_from_sq_test "d5 -> SE" "d5" SE
        [ "e4"; "f3"; "g2"; "h1" ];
      iterator_from_sq_test "d5 -> S" "d5" S [ "d4"; "d3"; "d2"; "d1" ];
      iterator_from_sq_test "d5 -> SW" "d5" SW [ "c4"; "b3"; "a2" ];
      iterator_from_sq_test "d5 -> W" "d5" W [ "c5"; "b5"; "a5" ];
      iterator_from_sq_test "d5 -> NW" "d5" NW [ "c6"; "b7"; "a8" ];
      iterator_from_sq_test "d8 -> W" "d8" N [];
      iterator_from_sq_test "h8 -> NE" "h8" NE [];
      iterator_from_sq_test "h4 -> E" "h4" E [];
      iterator_from_sq_test "h1 -> SE" "h1" SE [];
      iterator_from_sq_test "d1 -> S" "d1" S [];
      iterator_from_sq_test "a1 -> SW" "a1" SW [];
      iterator_from_sq_test "a4 -> W" "a4" W [];
      iterator_from_sq_test "a8 -> N" "a8" NW [];
      iterator_from_sq_test "h7 -> NW" "h7" NW [ "g8" ];
      iterator_from_sq_test "d4 -> L" "d4" L
        [ "e6"; "f5"; "f3"; "e2"; "c2"; "b3"; "b5"; "c6" ];
      iterator_from_sq_test "a1 -> L" "a1" L [ "b3"; "c2" ];
      iterator_from_sq_test "b5 -> L" "b5" L
        [ "c7"; "d6"; "d4"; "c3"; "a3"; "a7" ];
      (* values_captured tests *)
      (* TODO: implement captured pieces to be a non-empty list *)
      value_captured_test "Black has pinned piece but is not in check"
        Black 0;
      value_captured_test "Black has pinned piece but is not in check"
        White 0;
      value_captured_test "Black in check East" Black 0;
      value_captured_test "Black in check East" White 0;
      value_captured_test "Various pieces can intercept" Black 0;
      value_captured_test "Various pieces can intercept" White 0;
      value_captured_test
        "Pawn attack and initial one or two space move" White 0;
      value_captured_test
        "Pawn attack and initial one or two space move" Black 0;
      value_captured_test "Double check, L and SE, Queen" Black 0;
      value_captured_test "Double check, L and SE, Queen" White 0;
      value_captured_test "Full range of pawn attack" Black 0;
      value_captured_test "Full range of pawn attack" White 0;
    ];
  ]

(** [parse_test name str board expected] constructs an OUnit test named
    [name] that asserts the equality of [expected] with
    [parse str board]. *)
let parse_test name str board expected : test =
  name >:: fun _ -> assert_equal expected (parse str board)

(** [parse_invalid_squares_test name str board] constructs an OUnit test
    named [name] that asserts [parse str board] raises [InvalidSquares]. *)
let parse_invalid_squares_test name str board : test =
  name >:: fun _ ->
  let f () = parse str board in
  assert_raises InvalidSquares f

(** [parse_inconsistent_test name str board] constructs an OUnit test
    named [name] that asserts [parse str board] raises
    [InconsistentPlacement]. *)
let parse_inconsistent_test name str board : test =
  name >:: fun _ ->
  let f () = parse str board in
  assert_raises InconsistentPlacement f

(** [parse_malformed_test name str board] constructs an OUnit test named
    [name] that asserts [parse str board] raises [Malformed]. *)
let parse_malformed_test name str board : test =
  name >:: fun _ ->
  let f () = parse str board in
  assert_raises Malformed f

let command_tests =
  [
    parse_test "move P d2 to d3 -> Move [P; d2; to; d3]"
      "move P d2 to d3" board
      (Move [ "P"; "d2"; "to"; "d3" ]);
    parse_test "  move P   d2 to   d3 -> Move [P; d2; to; d3]"
      "  move P   d2 to   d3" board
      (Move [ "P"; "d2"; "to"; "d3" ]);
    parse_test "move N b1 to a3 -> Move [N; b1; to; a3]"
      "move N b1 to a3" board
      (Move [ "N"; "b1"; "to"; "a3" ]);
    parse_test "move P d7 to d5 -> Move [P; d7; to; d5]"
      "move P d7 to d5" board
      (Move [ "P"; "d7"; "to"; "d5" ]);
    parse_malformed_test "empty raises Malformed" "" board;
    parse_malformed_test "go P d2 to d3 raises Malformed"
      "go P d2 to d3" board;
    parse_malformed_test "move P d2 d3 raises Malformed" "move P d2 d3"
      board;
    parse_malformed_test "move W d2 to d3 raises Malformed"
      "move W d2 to d3" board;
    parse_malformed_test "move P d2 to d3 a raises Malformed"
      "move P d2 to d3 a" board;
    parse_invalid_squares_test "move P d2 to d9 raises InvalidSquares"
      "move P d2 to d9" board;
    parse_invalid_squares_test "move P d9 to d2 raises InvalidSquares"
      "move P d9 to d2" board;
    parse_invalid_squares_test "move P l2 to d4 raises InvalidSquares"
      "move P l2 to d4" board;
    parse_invalid_squares_test "move P d4 to l2 raises InvalidSquares"
      "move P d4 to l2" board;
    parse_inconsistent_test
      "move K d2 to d3 raises InconsistentPlacement" "move K d2 to d3"
      board;
    parse_inconsistent_test
      "move P a1 to a8 raises InconsistentPlacement" "move P a1 to a8"
      board;
  ]

(** [valid_moves_test name expected] constructs an OUnit test named
    [name] that asserts the equality of the move list generated by
    [valid_moves color (init_from_json name)] and [expected]. *)
let valid_moves_test name =
  "valid_moves_test" ^ name >:: fun _ ->
  let test = List.assoc name tests in
  let board = init_from_fen test.fen in
  let computed_moves = valid_moves board in
  assert_equal
    (List.filter (fun x -> not (List.mem x test.moves)) computed_moves)
    [] ~printer:string_of_string_tup_list;
  assert_equal
    (List.filter (fun x -> not (List.mem x computed_moves)) test.moves)
    [] ~printer:string_of_string_tup_list

(** [valid_piece_moves_test name sq expected] constructs an OUnit test
    named [name] that asserts the equality of the move list generated by
    [valid_piece_moves board p] and [expected]. *)
let valid_piece_moves_test name sq expected =
  "valid_piece_moves_test" ^ name >:: fun _ ->
  let board = init_from_fen (List.assoc name tests).fen in
  let p =
    match piece_of_square board sq with
    | None -> failwith "bad test"
    | Some p' -> p'
  in
  let computed_moves = valid_piece_moves board p in
  assert_equal
    (List.filter (fun x -> not (List.mem x expected)) computed_moves)
    [] ~printer:string_of_string_tup_list;
  assert_equal
    (List.filter (fun x -> not (List.mem x computed_moves)) expected)
    [] ~printer:string_of_string_tup_list

let check_printer = function
  | NotCheck -> "Not Check"
  | Check dirs -> pp_dirs dirs

(** [is_check_test name] constructs an OUnit test named [name] that
    asserts the correctness of the check state for test [name]. *)
let is_check_test name =
  name >:: fun _ ->
  let test = List.assoc name tests in
  let board = init_from_fen test.fen in
  let check_state = get_checks board in
  assert_equal check_state test.check ~printer:check_printer

let is_draw_test name =
  name >:: fun _ ->
  let test = List.assoc name tests in
  let board = init_from_fen test.fen in
  let draw = is_draw board in
  assert_equal draw test.draw ~printer:string_of_bool

let is_check_tests =
  let test_name t = match t with name, _ -> name in
  tests |> List.map test_name |> List.map is_check_test

let is_draw_tests =
  let test_name t = match t with name, _ -> name in
  tests |> List.map test_name |> List.map is_draw_test

let is_checkmate_test name expected =
  "valid_piece_moves_test" ^ name >:: fun _ ->
  let board = init_from_fen (List.assoc name tests).fen in
  let checkmate = is_checkmate board in
  assert_equal checkmate expected ~printer:string_of_bool

let is_stalemate_test name expected =
  "valid_piece_moves_test" ^ name >:: fun _ ->
  let board = init_from_fen (List.assoc name tests).fen in
  let stalemate = is_stalemate board in
  assert_equal stalemate expected ~printer:string_of_bool

let best_move_test name expected =
  "valid_piece_moves_test" ^ name >:: fun _ ->
  let fen = (List.assoc name tests).fen in
  let move = best_move fen "3000" in
  assert_equal move expected

let valid_moves_tests =
  let test_name t = match t with name, _ -> name in
  tests |> List.map test_name |> List.map valid_moves_test

let valid_piece_moves_tests =
  [
    (* valid piece moves tests *)
    valid_piece_moves_test "King can not deliver a pin" "e5"
      [ ("e5", "d4"); ("e5", "e4") ];
    valid_piece_moves_test "Pawn can not deliver a pin" "f7"
      [ ("f7", "f6"); ("f7", "f5") ];
    valid_piece_moves_test "Take piece to intercept" "d3"
      [ ("d3", "e4") ];
    valid_piece_moves_test "En-passant white on black" "c5"
      [ ("c5", "c6"); ("c5", "b6") ];
    valid_piece_moves_test "En-passant black on white" "b4"
      [ ("b4", "b3"); ("b4", "a3") ];
    valid_piece_moves_test "Not En-passant, but in the same position"
      "b4" [ ("b4", "b3") ];
    valid_piece_moves_test
      "Overlooked En-passant left, new En-passant right" "b5"
      [ ("b5", "b6"); ("b5", "c6") ];
    valid_piece_moves_test
      "Not En-passant, but in the same position on both sides" "b5"
      [ ("b5", "b6") ];
    valid_piece_moves_test "Knight move blocking castle" "e1"
      [ ("e1", "d2") ];
    valid_piece_moves_test
      "Where the spot in between a king castle is under attack" "e1"
      [ ("e1", "e2"); ("e1", "f2"); ("e1", "f1") ];
    valid_piece_moves_test "Black King can castle to g8" "e8"
      [ ("e8", "d8"); ("e8", "f8"); ("e8", "g8") ];
    valid_piece_moves_test
      "Black King cannot castle because rook has moved" "e8"
      [ ("e8", "f8"); ("e8", "d8") ];
    valid_piece_moves_test
      "King cannot castle on either side because king has moved" "e8"
      [ ("e8", "d8"); ("e8", "f8") ];
    valid_piece_moves_test
      "Black king can’t castle; in check from knight" "e8"
      [ ("e8", "e7"); ("e8", "f8") ];
    valid_piece_moves_test "Valid moves post castling for White" "g1"
      [ ("g1", "h1") ];
  ]

let game_end_tests =
  [
    is_checkmate_test "checkmate" true;
    is_stalemate_test "stalemate" true;
    is_checkmate_test "stalemate" false;
    is_stalemate_test "checkmate" false;
  ]

let engine_tests =
  [
    best_move_test "Capture a piece to prevent a check"
      (("h8", "g8"), None);
    best_move_test "En-passant Discovered Checkmate" (("f5", "g6"), None);
    best_move_test "pawn promotion to win" (("h7", "h8"), Some Queen);
  ]

let fen_test name =
  let fen = (List.assoc name tests).fen in
  let board = init_from_fen fen in
  let fen' = export_to_fen board in
  (* let board' = init_from_fen fen' in *)
  (* let fen'' = export_to_fen board' in *)
  name ^ "a" >:: fun _ -> assert_equal fen fen' ~printer:(fun x -> x)

let fen_tests =
  let test_name t = match t with name, _ -> name in
  tests |> List.map test_name |> List.map fen_test

let suite =
  "test suite for chess"
  >::: List.flatten
         [
           List.flatten board_tests;
           command_tests;
           valid_moves_tests;
           valid_piece_moves_tests;
           is_check_tests;
           fen_tests;
           is_draw_tests;
           game_end_tests;
           engine_tests;
         ]

let _ = run_test_tt_main suite
