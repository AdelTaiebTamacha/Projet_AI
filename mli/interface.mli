(** Interface manage the display features and all the algorithm call of the other modules *)



val width : int
(** The window width size *)

val height : int
(** The window height size *)

val window_x : int
(** The window horizontal position *)

val window_y : int
(** The window vertical position *)

(** *)

val x : int ref
(** The car horizontal position *)

val y : int ref
(** The car vertical position *)

val angle : float ref
(** The car angle position *)

val delta : int ref
(** The car linear displacement value *)

val deltb : float ref
(** The car angular displacement value *)

val lamda : int ref
(** The view zoom factor*)

val circuit : Type.portion_float array
(** The circuit *)

(** *)

val pi : float
(** Mathematic constant Pi *)

val resize : int * int -> int * int
(** [resize couple] Homothety of lamda factor *)

val draw_car : int -> int -> float -> unit
(** [draw_car x y theta] Draw the car at the position (x,y) with the theta angle*)

val draw_portion : Type.portion_int -> unit
(** [draw_portion potion] Draw the given portion (which coordinates are include in the portion type) *)

val draw_circuit : 'a -> unit
(** [draw_circuit ()] Take no parameter, draw the circuit defined by the global circuit *)

val draw_bg : 'a -> unit
(** [draw_circuit ()] Take no parameter, draw the background of the screen *)

(** *)

val key_handler : Graphics.status -> char -> unit
(** [key_handler status charcacter] Garbage collector - only print the given character with the given status*)

val print_status : 'a -> unit
(** [print_status]  Take no parameter, draw the status information on the screen *)
