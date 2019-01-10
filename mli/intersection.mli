(** Intersection manage geometric calculus in the plane*)


val ( -- ) : int * int -> int * int -> int * int
(**[p1--p2] Tuple subtraction*)

val ( ++ ) : int * int -> int * int -> int * int
(**[p1++p2] Tuple addition*)

val cross : int * int -> int * int -> int
(**[cross p1 p2] Tuple vector product*)

val dot : int * int -> int * int -> int
(**[dot p1 p2] Tuple scalar product*)

val sign : int -> int
(**[sign x]Return sign of int (+) -> 1; (null) -> 0; (-) -> -1;*)

val line_side_test : int * int -> int * int -> int * int -> int
(**[line_side_test p1 p2 p3] Return the relative position of p3 compared to the straight through p1 and p2
Right -> +1
On the straight -> 0
Left -> -1*)

val segments_intersect :
  (int * int) * (int * int) -> (int * int) * (int * int) -> bool
(**[segments_intersect (p1,p2) (p3,p4)] Return if the segment (p1,p2) and (p3,p4) intersect one each other*)

