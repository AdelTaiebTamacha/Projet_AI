(** Conversion module is mainly for recovering and convertion of cuircuit elements *)



val fichier : string
(**The circuit .svg file*) 

val split_on_char : char -> string -> string list
(**[split_on_char separator string] Split the string according to the separator*) 

val trad : string -> float * float
(**########################################*) 

val print_txt : string -> unit
(**[trad text] Surfomated printing text*) 

val print_tuple : float * float -> unit
(**[print_tuple (a,b)] Print the tuple coordinates on a single line *) 

val somme : float * float -> float * float -> float * float
(** [somme (a,b) (c,d)] Return the addition of the two given tuple (a+c,b+d) *) 

val is_digit_or_moins : char -> bool
(**[is_digit_or_moins character] Return true if the character is in (1 to 9,-), return false otherwise*) 

val compte_num : int -> string list -> int
(** [compte_num index list] Return the occurrence number of element in (1 to 9,-) from the given list *) 

val val_coupe : string list -> int
(**########################################*)

val couple_int_of_float : float * float -> int * int
(** [couple_int_of_float (a,b)] Convert the given tuple of floating coordinates into integer coordinates*) 

val portion_int_of_float : Type.portion_float -> Type.portion_int
(** [portion_int_of_float portion] Convert the given portion of floating coordinates into integer coordinates*) 

val print_portion_float : Type.portion_float -> unit
(** [print_portion_float portion] Print the coordinates of the given portion of floating coordinates*) 

val print_portion_int : Type.portion_int -> unit
(** [print_portion_int portion] Print the coordinates of the given portion of integer coordinates*) 

val print_list : string list -> unit
(** [print_list list] Print the given list*) 

val print_tab_float : Type.portion_float array -> unit
(** [print_tab_float table] Print the given table of portion of floating coordinates*) 

val print_tab_int : Type.portion_float array -> unit
(** [print_tab_int table] Print the given table of portion of integer coordinates*) 

val coordfun : string list -> string list
(**########################################*)


val conv : 'a -> string list
(** [conv ()] Convert the .svg circuit file into a circuit list of sting coordinates *) 

val trapeze : string list -> Type.portion_float array
(** [trapeze list] Convert the given list of string coordinates into a circuit table of portion of floating coordinates*) 

val main : 'a -> unit
(** [main ()] For debug purpose only : print the list version, table of floating coordinates version and table of integer coordinates of the cuitcuit*) 

