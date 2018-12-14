open Graphics;;
(*open Convert;;*)
open Printf;;


printf "Welcome to AI interface REV 1.0";;


open_graph "500x500-100x100";;

set_window_title "AI interface REV 1.0";;


set_color black;;
let points = [| 10,10 ; 100,10 ; 100,100 ; 10,100 |] in
	draw_poly points;;


let s = Graphics.wait_next_event [Button_down];;

clear_graph;;

let i = ref 1 in
while true do
	Graphics.wait_next_event [Button_down];
	draw_circle 250 250 (!i*10);
	i:=!i+1
done;;


let s = Graphics.wait_next_event [Button_down];;

close_graph;;