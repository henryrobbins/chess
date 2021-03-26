open OUnit2
open Board

let board = init_from_json (Yojson.Basic.from_file "board_init.json")

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

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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

(** [pp_pair pp1 pp2 (a, b)] pretty-prints [(a, b)] using
    [pp1] for [a] and [pp2] for [b]. *)
let pp_pair pp1 pp2 (a, b) =
  "(" ^ pp1 a ^ ", " ^ pp2 b ^ ")"

(** [move_piece_test name b s s'] constructs OUnit tests named [name] that
    assert [move_piece b (piece_of_square s) s'] is correct. *)
let move_piece_test name b s s' : test list =
  let p = piece_of_square b s in
  let b' = move_piece b p s' in
  [ (name ^ " | previous board sqaure" >:: fun _ ->
    assert_equal None (piece_of_square b' s));
    (name ^ " | new board sqaure (piece id) " >:: fun _ ->
      assert_equal (id_of_piece (p)) (id_of_piece (piece_of_square b' s')));
    (name ^ " | new board sqaure (piece color) " >:: fun _ ->
      assert_equal (color_of_piece (p)) (color_of_piece (piece_of_square b' s')));
    (name ^ " | new board sqaure (piece pos) " >:: fun _ ->
      assert_equal (square_of_piece (piece_of_square b' s')) (Some s'));]

(** [iterator_from_sq_test name board s d expected] constructs an OUnit test
    named [name] that asserts the equality of [expected]
    with [iterator_from_sq s d]. *)
let iterator_from_sq_test name s d expected : test =
  name >:: fun _ ->
  assert_equal expected (iterator_from_sq s d) ~printer:(pp_list Fun.id)

let board_tests =
  [ (* move_pieces tests *)
    (* TODO: add test cases for captured pieces. *)
    move_piece_test "d2 -> d4" board "d2" "d4";
    move_piece_test "e7 -> e5" board "e7" "e5";
    move_piece_test "g1 -> h3" board "g1" "h3";

    (* iterator_from_sq tests *)
    (* TODO: add test cases for L direction. *)
    [ iterator_from_sq_test "d5 -> N" "d5" N ["d6"; "d7"; "d8"];
      iterator_from_sq_test "d5 -> NE" "d5" NE ["e6"; "f7"; "g8"];
      iterator_from_sq_test "d5 -> E" "d5" E ["e5"; "f5"; "g5"; "h5"];
      iterator_from_sq_test "d5 -> SE" "d5" SE ["e4"; "f3"; "g2"; "h1"];
      iterator_from_sq_test "d5 -> S" "d5" S ["d4"; "d3"; "d2"; "d1"];
      iterator_from_sq_test "d5 -> SW" "d5" SW ["c4"; "b3"; "a2"];
      iterator_from_sq_test "d5 -> W" "d5" W ["c5"; "b5"; "a5"];
      iterator_from_sq_test "d5 -> NW" "d5" NW ["c6"; "b7"; "a8"];
      iterator_from_sq_test "d8 -> W" "d8" N [];
      iterator_from_sq_test "h8 -> NE" "h8" NE [];
      iterator_from_sq_test "h4 -> E" "h4" E [];
      iterator_from_sq_test "h1 -> SE" "h1" SE [];
      iterator_from_sq_test "d1 -> S" "d1" S [];
      iterator_from_sq_test "a1 -> SW" "a1" SW [];
      iterator_from_sq_test "a4 -> W" "a4" W [];
      iterator_from_sq_test "a8 -> N" "a8" NW []; ]]

let suite =
  "test suite for chess"
  >::: List.flatten [ (List.flatten board_tests) ]

let _ = run_test_tt_main suite
