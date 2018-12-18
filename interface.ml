open Graphics;;
(*open Convert;;*)
open Printf;;
open Init;;

Random.init;;

let width = 700;;
let height = 500;;
let window_x = 100;;
let window_y = 100;;

let x = ref 100;;
let y = ref 100;;
let angle = ref 0.;;
let delta = ref 10;;

open_graph (sprintf "%dx%d-%dx%d" width height window_x window_y);;
set_window_title "AI interface REV 1.2";;
clear_graph ();;

Init.draw_init ();;


type point = {x : float; y : float};;
let pi = 4.0 *. atan 1.0;;



let draw_bg = fun _ ->
	moveto 10 20;
	draw_string "Right click or hjkl to move car";
	moveto 10 8;
	draw_string "Press q exit";;

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

let key_handler = fun s c ->
	moveto s.mouse_x s.mouse_y;
	draw_char c;;

let print_status = fun _ ->
	moveto (size_x ()- 150) 10;
	draw_string (sprintf "Coord : %d %d" !x !y);;



let condi = ref true in
while !condi do


	let s = wait_next_event[Button_down; Key_pressed] in
	clear_graph ();
	draw_bg ();
	print_status ();
	if s.button then begin
		x := s.mouse_x;
		y := s.mouse_y;
		angle := (Random.float 360.);
		draw_car !x !y !angle;
		end
	else if s.keypressed then begin
		let c = s.key in
		match c with
		'q' | 'Q' -> condi := false;
		| 'h' -> x := !x - !delta; draw_car !x !y !angle;
		| 'l' -> x := !x + !delta; draw_car !x !y !angle;
		| 'j' -> y := !y - !delta; draw_car !x !y !angle;
		| 'k' -> y := !y + !delta; draw_car !x !y !angle;
		| _ -> key_handler s c;
		
		end
		
done;;




close_graph;;