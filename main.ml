open Board
open Command

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
                let board' = move_piece board p sq' in
                interact board'
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
