open GMain
open GdkKeysyms
open Board

(* Static parameters for the GUI *)
let width = 250
let height = 250

(* Connect a signal handler, ignoring the resulting signal ID.
   This avoids having to use [|> ignore] everywhere.
   https://stackoverflow.com/questions/63106011 *)
let (==>) (signal:(callback:_ -> GtkSignal.id)) callback =
   ignore (signal ~callback)

let locale = GtkMain.Main.init ()

let main () =
  let window = GWindow.window ~width:width ~height:height
                              ~position: `CENTER
                              ~resizable: false
                              ~title:"OCaml Chess" () in
  let table = GPack.table ~packing:window#add () ~homogeneous: true
                          ~rows: 8 ~columns: 8
                          ~width: width ~height: height in
  window#connect#destroy  ==> Main.quit;

  (* state variables *)
  let board = ref (init_game ()) in
  let choose_from = ref true in
  let from_sq = ref None in
  let to_sq = ref None in

  (* construct button matrix *)
  let rec button_matrix rows cols i j btns =
    match rows with
    | [] -> btns;
    | r :: rt ->
      match cols with
      | [] -> if rt = [] then btns else button_matrix rt ranks (i + 1) 0 btns
      | c :: ct ->
        let id = print_piece (piece_of_square !board (r^c)) in
        let test x = table#attach i (7 - j) x in
        let button = GButton.button ~label:id ~packing:test () in
        button#set_relief `NONE;
        let btns = (r^c, button) :: btns in
        button_matrix (r :: rt) ct i (j + 1) btns in

  let buttons = button_matrix files ranks 0 0 [] in

  (* go back and set all the button callbacks *)
  let rec set_callbacks rows cols =
    match rows with
    | [] -> ();
    | r :: rt ->
      match cols with
      | [] -> if rt = [] then () else set_callbacks rt ranks
      | c :: ct ->
        let button = List.assoc (r^c) buttons in
        (* button#connect#enter  ==> (fun () -> prerr_endline (r^c)); *)
        button#connect#pressed  ==> (fun () ->
          if !choose_from then
            (from_sq := Some (r^c); to_sq := None)
          else
            to_sq := Some (r^c);

          choose_from := not !choose_from;

          if !choose_from then
            (* get current from/to square and buttons *)
            let a =
              match !from_sq with
              | None -> failwith "no from sqaure selected"
              | Some sq -> sq in
            let b =
              match !to_sq with
              | None -> failwith "no to square selected"
              | Some sq -> sq in
            let from_btn = List.assoc a buttons in
            let to_btn = List.assoc b buttons in

            (* update *)
            let p = piece_of_square !board a in
            match p with
            | None -> print_endline "no piece here"
            | Some p ->
              (* update board *)
              board := move_piece !board p b;
              (* update buttons *)
              from_btn#set_label "";
              let p_id = print_piece (Some p) in
              to_btn#set_label p_id;
              print_endline ("move "^p_id^" "^a^" to "^b);
          );
        set_callbacks (r :: rt) ct in

  set_callbacks files ranks;

  window#show ();
  Main.main ()

let () = main ()
