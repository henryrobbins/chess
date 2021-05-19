(* Variations on Jacques Garrigue's lablgtk2 example testdnd.ml*)

(* this is a translation in Caml of the gtk+ example testdnd.c  *)


(* http://www.gtk.org/tutorial/ch-dragangdrop.html#SEC-DRAGANDDROPOVERVIEW

a typical drag-and-drop cycle would look as follows:
1 Drag begins.
2 Drag data request (when a drop occurs).
3 Drop data received (may be on same or different application).
4 Drag data delete (if the drag was a move).
5 Drag-and-drop procedure done.
*)

open Gaux
open Gtk
open GObj
open GMain

(*.............................................................*)

let dragIconPixmap =
(GDraw.pixmap_from_xpm ~file:"./draggableitem.xpm" ());;

let draggedIconPixmap =
(GDraw.pixmap_from_xpm ~file:"./draggeditem.xpm" ());;

let openTrashcanIconPxm =
(GDraw.pixmap_from_xpm ~file:"./trashcan_open.xpm" ());;

let closedTrashcanIconPxm =
(GDraw.pixmap_from_xpm ~file:"./trashcan_closed.xpm" ());;

let motionIndicatorArray = [|"/";"\\";"_"|];;
let motionIndicatorArrayLen = Array.length motionIndicatorArray;;
let motionIndicatorRefCounter = ref 0;;

(*.............................................................*)

let window1 = GWindow.window ~title:"DnD Sources Plus One Receiver" (*~width:300 ~height:150*) ();;
let _ = window1#misc#realize ();;

let window2 = GWindow.window ~title:"DnD Receiver" ~height:150 ();;
let _ = window2#misc#realize ();;


(*.............................................................*)
(* "You will need to set up your destination widgets to receive certain drag
 * and drop types by specifing a name and/or number. It would be more reliable
 * to use a name since another application may just happen to use the same
 * number for for an entirely different meaning."
 *)

let (dndTargets:(Gtk.target_entry) list) =
  [
    { target = "A_STRING"; flags = []; info = 0 };
    { target = "A_STRING_OF_FLOAT"; flags = []; info = 1}
  ];;

(*.............................................................*)
(*let's start with a new, clean handler*)

class neutralDragHandler =

object

  method private beginning (_ : drag_context) =
    ()
  method private data_delete (_ : drag_context) =
    ()
  method private data_get (_ : drag_context) (_ : selection_context) ~info:(_ : int) ~(time: int32) =
    ()
  method private data_received (_ : drag_context) ~(x: int) ~(y: int) (_ : selection_data) ~(info:int) ~(time: int32) =
    ()
  method private drop (_ : drag_context) ~(x: int) ~(y: int) ~(time: int32) =
    false
  method private ending (_ : drag_context) =
    ()
  method private leave (_ : drag_context) ~(time: int32) =
    ()
  method private motion (_ : drag_context) ~(x: int) ~(y: int) ~(time: int32) =
    false

end;;

(*.............................................................*)

class trashcanTargetForDrag ?packing ?show () =

  let (thrashcanPixmap:GMisc.image) =
    GMisc.pixmap closedTrashcanIconPxm ?packing ?show () in

object (self)

  inherit widget (thrashcanPixmap#as_widget)

  inherit neutralDragHandler

  val mutable have_drag = false


  method leave _ ~time =
    print_endline "Item leaving thrashcan area"; flush stdout;
    have_drag <- false;
    thrashcanPixmap#set_pixmap closedTrashcanIconPxm

  method motion (context:GObj.drag_context) ~(x:int) ~(y:int) ~(time:int32) =

    let () = GtkBase.DnD.highlight (thrashcanPixmap#as_widget) in (*can't see this working*)


    let () =
      if (not have_drag) then (*our target should receive _one_ drag at a time*)
	(
	  have_drag <- true;
	  thrashcanPixmap#set_pixmap openTrashcanIconPxm
	)
      else
	()
    in

    let () =
      let () =
	(Printf.printf
	   "%s"
	   motionIndicatorArray.((!motionIndicatorRefCounter mod motionIndicatorArrayLen ));
	 flush stdout)
      in
	incr motionIndicatorRefCounter
    in

    (* let () =
      context#status [context#suggested_action] ~time (* ??? *)
    in *)
    true

  method data_received aContext ~x ~y (receivedData:GObj.selection_data) ~info ~time =

    if (receivedData#format) = 8 then
      (* "The unit length of the data in bits (e.g. 8 for a string or 32 of an integer)"*)
      (
	let () =

	  let sourceTypeName (* what widget is the drag_source ? *) =
	    try
	      aContext#source_widget#misc#get_type
	    with Gpointer.Null -> "unknown"
	  in

	  (Printf.printf
	     "Trashcan received \"%s\" from a %s\n"
	     receivedData#data sourceTypeName ;
	   flush stdout)
	in
	  (aContext#finish
	     ~success:true
	     ~del:false
	     ~time
	  )
      )
    else
      (aContext#finish
	 ~success:false
	 ~del:false
	 ~time);

    method drop aContext ~x ~y ~time =
    (* this method contains a data_request to the source and is connected later on to the
    drag_drop signal *)
      let () =
	(prerr_endline "Dropping on trashcan"; flush stdout) in
      let () = (*free the flag*)
	have_drag <- false in

      let () =
	thrashcanPixmap#set_pixmap closedTrashcanIconPxm
      in
	false

  initializer

    (* GTK+ 2.0 Tutorial says: "A GDK_ACTION_COPY would be a typical drag-and-drop
     * without the source data being deleted
     * while GDK_ACTION_MOVE would be just like GDK_ACTION_COPY
     * but the source data will be 'suggested' to be deleted after
     * the received signal handler is called"*)
    thrashcanPixmap#drag#dest_set
      [(List.find (fun tgt -> (tgt.target = "A_STRING") ) dndTargets)]
      ~actions:[`COPY ];
    thrashcanPixmap#drag#connect#leave ~callback:self#leave;
    thrashcanPixmap#drag#connect#motion ~callback:self#motion;
    thrashcanPixmap#drag#connect#drop ~callback:self#drop;
    thrashcanPixmap#drag#connect#data_received ~callback:self#data_received;
    ()
end;;

(*.............................................................*)

class areaTargetForDrag ?packing ?show () =

  let refDataAccum = ref [] in

  let targetAreaLabel = GMisc.label ~text:"Drop Here\n" ?packing ?show () in

  object (self)

    inherit widget targetAreaLabel#as_widget

    inherit neutralDragHandler


    method drop aContext ~x ~y ~time =
    (* this method contains a data_request to the source and is connected later on to the
    drag_drop signal *)
    let () =
      (prerr_endline "Dropping on target area"; flush stdout) in

      false;

    method data_received aContext ~x ~y receivedData ~info ~time =
      (

	(
	  if (receivedData#format = 8) then
	    (
	      let () =
		(Printf.printf
		   "Target area received \"%s\"\n"
		   receivedData#data;
		 flush stdout
		)
	      in

	      let () =
		refDataAccum := ((float_of_string receivedData#data)::!refDataAccum)
	      in
	      let meanValNow =
		((List.fold_left (+.) 0. !refDataAccum) /.
		   (float (List.length !refDataAccum)) )
	      in
	      let droppedItemsStr =
		(List.fold_left
		   (fun a b -> (a^";"^(string_of_float b)))
		   ""
		   (List.rev !refDataAccum)
		)
	      in
	      let () = targetAreaLabel#set_text
			 ("You Have Just\nDropped "^
			  (receivedData#data)^
			  "\n\n So The Overall Dropped Items List is \n ["^
			  droppedItemsStr^
			  "] \n\nAverage Is Now "^
			  (string_of_float meanValNow))
	      in

		(aContext#finish
		   ~success:true
		   ~del:false
		   ~time
		)
	   )
	 else
	   (aContext#finish
	      ~success:true
	      ~del:false
	      ~time
	   )
	)
      )

    initializer
      targetAreaLabel#drag#dest_set
	[(List.find (fun tgt -> (tgt.target = "A_STRING_OF_FLOAT") ) dndTargets)]
	~actions:[`COPY ];
      targetAreaLabel#drag#connect#drop ~callback:self#drop;
      targetAreaLabel#drag#connect#data_received ~callback:self#data_received;

      ()
  end

(*.............................................................*)

class buttonSourceOfDrag ?packing ?show () =

  let aButton = GButton.button ?packing ?show () in
  let vbox = GPack.vbox ~border_width:2 ~packing:(aButton#add) ~show:true () in
  let buttonPxm =
    GMisc.pixmap
      dragIconPixmap
      ~packing:(vbox#pack ~padding:3 ~expand:true ~fill:true) ()
  in
  let buttonLabel = GMisc.label ~text:"Drag me\n" ~packing:(vbox#pack ~padding:3) () in


  let contentUpdateCounter = ref 0 in

  object (self)
    inherit widget (aButton#as_widget)
    inherit neutralDragHandler

    method beginning aContext =
      print_endline "Item dragged!"; flush stdout;

    method data_get _ sel ~info ~time =
      match info with
	| 0 -> sel#return "Oh! Here I land."
	| 1 -> let () = (incr contentUpdateCounter) in
	    sel#return ~format:8 (string_of_float ((float !contentUpdateCounter) *. 3.))
	| _ -> ()

  method data_delete _ =
    print_endline "Delete the data!"; flush stdout

  initializer

    (aButton#drag#source_set dndTargets ~modi:[`BUTTON1] ~actions:[`COPY ];
     aButton#drag#source_set_icon draggedIconPixmap;
     aButton#drag#connect#beginning ~callback:self#beginning;
     aButton#drag#connect#data_get ~callback:self#data_get;
     aButton#drag#connect#data_delete ~callback:self#data_delete;
     ()
    )

end


(*.............................................................*)

class daSourceOfDrag ~packing ?show () = (* we must provide packing because we're going to realize a widget *)

  (*for pixbuf, see pixview.ml by Jacques Garrigue*)
  let pixbuf = GdkPixbuf.from_file "./draggableitem.xpm"
  in

  let (pixmap, bitmapOption) = GdkPixbuf.create_pixmap pixbuf  in

  let width = GdkPixbuf.get_width pixbuf in
  let height = GdkPixbuf.get_height pixbuf in


  let da = GMisc.drawing_area ~width:width ~height:height ~packing ?show () in

  let dw = da#misc#realize (); new GDraw.drawable da#misc#window
  in

  let aCbID =
  da#event#connect#expose (fun _ -> dw#put_pixmap ~x:0 ~y:0 pixmap; true)
  in

  let contentUpdateCounter = ref 0 in

  object (self)
    inherit widget (da#as_widget)
    inherit neutralDragHandler


    method beginning aContext =
      print_endline "Item dragged!"; flush stdout;

    method data_get _ sel ~info ~time =
      match info with
	| 0 -> sel#return "I'm Data!"
	| 1 -> let () = (incr contentUpdateCounter) in
	    sel#return ~format:8 (string_of_float ((float !contentUpdateCounter) *. 3.))
	| _ -> ()

  method data_delete _ =
    print_endline "Delete the data!"; flush stdout

  initializer



    (
      da#drag#source_set dndTargets ~modi:[`BUTTON1] ~actions:[`COPY ];
      da#drag#source_set_icon draggedIconPixmap;
      da#drag#connect#beginning ~callback:self#beginning;
      da#drag#connect#data_get ~callback:self#data_get;
      da#drag#connect#data_delete ~callback:self#data_delete;
      ()
    )

  end
;;


(*.............................................................*)

let main () =

  let hbox = GPack.hbox ~homogeneous:true ~border_width:2 ~packing:(window1#add) ~show:true () in

  let aDnDSource =
    new buttonSourceOfDrag  ~packing:(hbox #add) () in

  let imageSourceVbox = GPack.vbox ~border_width:2 ~packing:(hbox#pack ~expand:true ~fill:true) ~show:true () in


  let imageSourceAlignment  = GBin.alignment ~xalign:1.0 ~packing:(imageSourceVbox #pack ~expand:true ~fill:true) () in

  let anotherDnDSource =
    new daSourceOfDrag ~packing:(imageSourceAlignment #add) () in


  let aDnDSourceLabel =
    GMisc.label ~text:"Can you\ndrag me too?\n"~packing:(imageSourceVbox #add) () in


  let aDnDTarget =
    new trashcanTargetForDrag ~packing:(hbox #add) () in

  let anotherDnDTarget =
    new areaTargetForDrag ~packing:(window2#add) () in

  let () =
    (
      window1#connect#destroy ~callback: Main.quit;
      window1#show ();

      window2#connect#destroy ~callback: Main.quit;
      window2#show ()
    )
  in

  Main.main ()
;;

(*.............................................................*)

let _ =
  main ()
;;
(*.............................................................*)
