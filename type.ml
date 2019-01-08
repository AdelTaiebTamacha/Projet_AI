(* ################################################################### *)
(*	                                                                   *)
(*                           TYPE DEFINITION                           *)
(*	                                                                   *)
(* ################################################################### *)


type portion_float = { ent_int : float*float;
						ent_ext : float*float; 
						sort_int : float*float; 
						sort_ext : float*float };;
						
type portion_int = { ent_int_int : int*int;
						ent_ext_int : int*int; 
						sort_int_int : int*int; 
						sort_ext_int : int*int };;