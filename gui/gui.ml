open GMain
open GdkKeysyms
open Board

(* Connect a signal handler, ignoring the resulting signal ID.
   This avoids having to use [|> ignore] everywhere.
   https://stackoverflow.com/questions/63106011 *)
let (==>) (signal:(callback:_ -> GtkSignal.id)) callback =
   ignore (signal ~callback)

let locale = GtkMain.Main.init ()

let main () =
  let window = GWindow.window ~width:500 ~height:500
                              ~position: `CENTER
                              ~resizable: false
                              ~title:"OCaml Chess" () in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy  ==> Main.quit;

  (* TODO: Make a grid of buttons *)
  let rec button_matrix rows cols =
    match rows with
    | [] -> ();
    | r :: rt ->
      match cols with
      | [] -> if rt = [] then () else button_matrix rt files
      | c :: ct ->
        let button = GButton.button ~label:(r^c) ~packing:vbox#add () in
        button#connect#clicked  ==> (fun () -> prerr_endline (r^c));
        button#set_xalign 0.5;
        button#set_yalign 0.5;
        button#set_relief `NONE;
        button_matrix (r :: rt) ct in

  button_matrix ranks files;

  window#show ();
  Main.main ()

let () = main ()
