open Board
open Command
open Validation

let rec interact board =
  print_game_state board;
  print_string "\n > ";
  match read_line () with
  | exception End_of_file -> ()
  | text -> (
      try
        let command = parse text board in
        match command with
        | Quit -> exit 0
        | Move move_phrase -> (
            match move_phrase with
            | [ id; sq; "to"; sq' ] ->
                let p = piece_of_square board sq in
                if is_valid_move (sq,sq') board then
                  let board' = move_piece board p sq' in
                  interact board'
                else
                  print_string "The move was invalid. Try again.";
                  interact board
            | _ -> () )
      with _ ->
        print_string
          "Commands must be of the form {move [id] [sq1] to [sq2]} or \
           {quit}. \n";
        interact board )

(** [main ()] prompts for the game to play, then starts it. *)
let main () = interact (init_game ())

(* Execute the game engine. *)
let () = main ()
