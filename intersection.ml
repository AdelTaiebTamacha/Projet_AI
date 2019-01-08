(* ################################################################### *)
(*	                                                                   *)
(*                        INTERSECTION FUNCTION                        *)
(*	                                                                   *)
(* ################################################################### *)


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




(* Test

let p1 = (10, 10)  ;;
let p2 = (50, 10) ;;
let p3 = (100, 10) ;; 

let p4 = (10, 50) ;; 
let p5 = (50, 50) ;;
let p6 = (100, 50) ;;

let p7 = (10, 100) ;;
let p8 = (50, 100) ;;
let p9 = (100, 100) ;;



if (segments_intersect (p1,p7) (p5,p3)) then 
	Printf.printf "inter"
else
	Printf.printf "no inter";;
	
Printf.printf "\n\n";;
*)