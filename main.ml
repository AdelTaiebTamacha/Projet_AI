(* 
To compile and run
ocamlc graphics.cma intersection.ml type.ml conversion.ml init.ml interface.ml main.ml -o main && rm -f *.cmi *.cmo && ./main

To run
./main or ./main.exe
*)

(* ################################################################### *)
(*	                                                                   *)
(*                                MAIN                                 *)
(*	                                                                   *)
(* ################################################################### *)
open Interface;;

let main = fun _ ->
	Interface.main_loop ();;
	
main ();;
	