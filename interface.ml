
open Graphics;;
(*open Convert;;*)
open Printf;;
open Init;;
open Conversion;;
open Type;;
open Projet;;
open Intersection;;

Random.init;;


(* ################################################################### *)
(*	                                                                   *)
(*                        INTERFACE SETTINGS                           *)
(*	                                                                   *)
(* ################################################################### *)

let width = 700;;
let height = 500;;
let window_x = 100;;
let window_y = 100;;

let x = ref 100;;
let y = ref 100;;
let angle = ref 0.;;
let delta = ref 10;;
let deltb = ref 10.;;

let lamda = ref 4;;



open_graph (sprintf "%dx%d-%dx%d" width height window_x window_y);;
set_window_title "AI interface REV 2.0";;
clear_graph ();;
Init.draw_init ();;

let circuit = create ();;
let solution = search circuit;;



let pi = 4.0 *. atan 1.0;;


(* ################################################################### *)
(*	                                                                   *)
(*                          DRAWING FUNCTION                           *)
(*	                                                                   *)
(* ################################################################### *)

let resize = fun couple ->
	let (a,b) = couple in 
	let a_ = a * !lamda in 
	let b_ = b * !lamda in
	(a_,b_);;


let draw_car = fun x y theta ->
	moveto x y;
	let l = 20. in
	let w = 10. in
	let a = theta *. pi /. 180.0 in
	let b = a +. pi /. 2.0 in
	let c = b +. pi /. 2.0 in
	rlineto	(int_of_float(l*.cos a)) (int_of_float(l*.sin a));
	rlineto (int_of_float(w*.cos b)) (int_of_float(w*.sin b));
	rlineto (int_of_float(l*.cos c)) (int_of_float(l*.sin c));
	lineto x y;;

let draw_portion = fun port ->	
	let (a,b) = port.ent_int_int in
	let (c,d) = port.ent_ext_int in
	let (e,f) = port.sort_ext_int in 
	let (g,h) = port.sort_int_int in
	let segment = Array.map resize [| (a,b); (c,d); (e,f); (g,h); (a,b) |] in
	if line_side_test (resize (a,b)) (resize (c,d)) (!x,!y) <= 0 &&
		line_side_test (resize (c,d)) (resize (e,f)) (!x,!y) <= 0 &&
		line_side_test (resize (e,f)) (resize (g,h)) (!x,!y) <= 0 &&		
		line_side_test (resize (g,h)) (resize (a,b)) (!x,!y) <= 0		
		then set_color green
	else set_color blue;
	fill_poly segment;
	set_color black;
	draw_poly segment;;
	(*Printf.printf "(%d,%d) (%d,%d) (%d,%d) (%d,%d)\n" a b c d e f g h;;*)
	
let draw_circuit = fun _ ->	
	set_color red;
	for i = 0 to (Array.length circuit -1) do
		draw_portion circuit.(i)
	done;
	set_color black;;
	

let draw_bg = fun _ ->
	moveto 10 36;
	draw_string "Zoom with i (in) and o (out)";
	moveto 10 22;
	draw_string "Right click or hjkl | bn to move car";
	moveto 10 8;
	draw_string "Press q exit";
	draw_circuit ();;

let draw_solution = fun _ ->
	let step = fun (x,y) ->	lineto x y in
	List.iter step solution;;
	
	

(* ################################################################### *)
(*	                                                                   *)
(*                        INTERFACE FUNCTION                           *)
(*	                                                                   *)
(* ################################################################### *)
	
let key_handler = fun s c ->
	moveto s.mouse_x s.mouse_y;
	draw_char c;;

let print_status = fun _ ->
	moveto (size_x ()- 150) 22;
	draw_string (sprintf "Zoom : %d" !lamda);
	moveto (size_x ()- 150) 8;
	draw_string (sprintf "Coord : %d %d" !x !y);;
	
	


	
(* ################################################################### *)
(*	                                                                   *)
(*                              MAIN LOOP                              *)
(*	                                                                   *)
(* ################################################################### *)
let main_loop = fun _ ->
	let car_update = ref true in
	let condi = ref true in
	while !condi do


		let s = wait_next_event[Button_down; Key_pressed] in
		clear_graph ();
		if s.button then begin
			x := s.mouse_x;
			y := s.mouse_y;
			angle := (Random.float 360.);
			car_update := true;
			end
		else if s.keypressed then begin
			let c = s.key in
			match c with
			'q' | 'Q' -> condi := false;
			| 'h' -> x := !x - !delta; car_update := true;
			| 'l' -> x := !x + !delta; car_update := true;
			| 'j' -> y := !y - !delta; car_update := true;
			| 'k' -> y := !y + !delta; car_update := true;
			| 'b' -> angle := !angle +. !deltb; car_update := true;
			| 'n' -> angle := !angle -. !deltb; car_update := true;
			| 'i' -> lamda := !lamda + 1;
			| 'o' -> lamda := !lamda - 1;
			| _ -> key_handler s c;
			
			end;
			
		draw_bg ();
		draw_solution ();
		
		
		if !car_update then draw_car !x !y !angle;
		car_update := false;
		
		print_status ();
			
	done;
	close_graph;;