(* ################################################################### *)
(*	                                                                   *)
(*                        INTERSECTION FUNCTION                        *)
(*	                                                                   *)
(* ################################################################### *)
open Type;;

let (--) (x1,y1) (x2,y2) = (x1-x2, y1-y2);;
let (++) (x1,y1) (x2,y2) = (x1+x2, y1+y2);;
let cross (x1,y1) (x2,y2) = (x1*y2) - (y1*x2);;
let dot (x1,y1) (x2,y2) = (x1*x2) + (y1*y2);;
let sign x = compare x 0;;
(* returns -1 if x<0, 0 if x=0 and 1 if x>0 *)


let line_side_test p1 p2 p3 = sign (cross (p2--p1) (p3--p1));;

let segments_intersect (a,b) (c,d) =
(line_side_test a b d) * (line_side_test a b c) <= 0 &&
(line_side_test c d a) * (line_side_test c d b) <= 0;;


let middle_start = fun port ->
	let (a,b) = port.ent_int_int in
	let (c,d) = port.ent_ext_int in	
	((a+c)/2, (b+d)/2);;