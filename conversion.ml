open Type;;

let fichier = "dessin3.svg"


(* ################################################################### *)
(*	                                                                   *)
(*                           STRING FUNCTION                           *)
(*	                                                                   *)
(* ################################################################### *)


let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r;;
let trad = fun text -> 
  let att = split_on_char ',' text in 
  let x = float_of_string (List.hd att)in 
  let y = float_of_string (List.hd (List.tl att))in
  (x,y);;
let print_txt = fun a -> Printf.printf " %s |" a;; 
let print_tuple = fun (x,y) -> Printf.printf "%f,%f\n" x y;;  
let somme = fun (x,y) (v,w) -> (x+.v,y+.w);;



(* ################################################################### *)
(*	                                                                   *)
(*                         CONVERTING FUNCTION                         *)
(*	                                                                   *)
(* ################################################################### *)

let couple_int_of_float = fun couple ->
	let (a,b) = couple in
	let couple_int = (int_of_float a, int_of_float b) in
	couple_int;;
	

let portion_int_of_float = fun port ->
	let port_int = 
		{ent_int_int = couple_int_of_float port.ent_int;
		 ent_ext_int = couple_int_of_float port.ent_ext;
		 sort_int_int = couple_int_of_float port.sort_int;
		 sort_ext_int = couple_int_of_float port.sort_ext} in
	port_int;;
	
	
(* ################################################################### *)
(*	                                                                   *)
(*                   PRINTING SUBFUNCTION - ELEMENT                    *)
(*	                                                                   *)
(* ################################################################### *)

	 
(* PORTION PRINT - FLOATING VALUE *)	
let print_portion_float = fun port ->
  let (a,b) = port.ent_int in
  let (c,d) = port.ent_ext in
  let (e,f) = port.sort_int in 
  let (g,h) = port.sort_ext in
  Printf.printf "(%f,%f) (%f,%f) (%f,%f) (%f,%f)\n" a b c d e f g h;;

    

(* PORTION PRINT - INTERGER VALUE *)	
let print_portion_int = fun port ->
	let (a,b) = port.ent_int_int in
	let (c,d) = port.ent_ext_int in
	let (e,f) = port.sort_int_int in 
	let (g,h) = port.sort_ext_int in
	Printf.printf "(%d,%d) (%d,%d) (%d,%d) (%d,%d)\n" a b c d e f g h;;


(* Function that will validate if the input is a ALPHA *)
let is_digit_or_moins = fun digit ->
  match digit with
      '0' .. '9' -> true;
    |'-' -> true;
    | _ -> false;;
    
let rec compte_num = fun i liste -> 
  match liste with 
    |tete::queue->
      if (is_digit_or_moins ((List.hd liste).[0])) then (compte_num (i+1) queue)
      else compte_num i queue;
    |[]-> i;;
    
let val_coupe = fun liste -> 
  let limite = (compte_num 0 liste)/2 in 
  let rec val_coupe_rec = fun compteur_num compteur_mot liste ->
    match liste with
      |tete::queue-> 
        if (is_digit_or_moins (List.hd liste).[0])then  
          let c_num = compteur_num + 1 in
          let c_mot = compteur_mot + 1 in 
          if (c_num == limite) then c_mot
          else val_coupe_rec c_num c_mot queue;
        else val_coupe_rec compteur_num (compteur_mot+1) queue;
      |[]->0 in 
  val_coupe_rec 0 0 liste;;


	
(* ################################################################### *)
(*	                                                                   *)
(*                   PRINTING FUNCTION - GATHERING                     *)
(*	                                                                   *)
(* ################################################################### *)

(* LIST PRINT - STRING VALUE *)
let rec print_list = function 
	[] -> ()
	| e::l -> print_string e ; print_string " \n" ; print_list l;;

	
(* TABLE PRINT - FLOATING VALUE *)	
let print_tab = fun tab ->
	for i = 0 to (Array.length tab -1) do
		print_portion_float tab.(i)
	done;;


(* TABLE PRINT - INTERGER VALUE *)
let print_tab_int = fun tab ->
	for i = 0 to (Array.length tab -1) do
		let int_portion = portion_int_of_float tab.(i) in
		print_portion_int int_portion
	done;;
	


(* ################################################################### *)
(*	                                                                   *)
(*                        COORDINATE RECOVERY                          *)
(*	                                                                   *)
(* ################################################################### *)

let coordfun = fun list ->
  let rec coord_rec = fun list coor ->
    match list with
        "z\""::queue -> coor;
      |"Z\""::queue -> coor;
      |tete::queue ->let coor = tete::coor in
                     coord_rec queue coor;
      |_ ->["fin"] in
  coord_rec list [];;


let conv = fun _ ->
  let fic = open_in fichier in

  let rec conv_rec coord_list =
    try
      let ligne = input_line fic in
      let lignesep = split_on_char ' ' ligne in
     (* List.iter print_txt lignesep;
      print_string "\n";*)
      let rec recherche = fun lsep ->
        match lsep with
            "d=\"m"::queue ->List.rev (coordfun queue); 
          |"</svg>"::_ -> [];
          |tete::queue -> recherche queue;
          |_ -> [] in 
      let ligne_list = recherche lignesep in 
      conv_rec (ligne_list)@coord_list;  
    with End_of_file -> close_in fic;coord_list in
  conv_rec [];;

let trapeze = fun list -> 
  let milieu = ref (val_coupe list) in
  let trap_tab = Array.make !milieu  {ent_int= (0.,0.);
                                      ent_ext = (0.,0.);
                                      sort_int = (0.,0.);
                                      sort_ext = (0.,0.)} in
  let interieur = ref [] in
  let exterieur = ref [] in 
  for i = 0 to (!milieu-1) do
    exterieur := (List.nth list i)::(!exterieur);
  done;
  for i = !milieu to (List.length list - 1) do
    interieur := (List.nth list  i)::(!interieur);
  done;
  (*if (List.length !interieur) != (List.length !exterieur) then begin
    print_string "Les deux parties n'ont pas la bonne longueur !!!\n";
    exit 0;
  end;*)
  interieur := List.rev !interieur;
  exterieur := List.rev !exterieur;
  List.iter print_txt !interieur;
  Printf.printf "%d points" (List.length !interieur);
  print_string "\n";
  List.iter print_txt !exterieur;
  Printf.printf  "%d points" (List.length !exterieur);
  print_string "\n";
  let rec trace_points = fun mode acc liste_points liste ->
    match liste  with
      |[]-> List.rev liste_points;
      |"L"::queue -> trace_points 'L' acc liste_points queue;
      |"l"::queue -> trace_points 'l' acc liste_points queue;
      |"v"::queue -> trace_points 'v' acc liste_points queue;
      |"V"::queue -> trace_points 'V' acc liste_points queue;
      |"h"::queue -> trace_points 'h' acc liste_points queue;
      |"H"::queue -> trace_points 'H' acc liste_points queue;
      |tete::queue -> match mode with 
          |'L'-> let racc = trad tete in
                 let liste_points = racc::liste_points in
                 trace_points mode racc liste_points queue
          |'v'-> let racc = (somme (0.,(float_of_string tete)) acc) in
                 let liste_points = racc::liste_points in
                 trace_points mode racc liste_points queue
          |'h'-> let racc = (somme ((float_of_string tete),0.) acc) in
                 let liste_points = racc::liste_points in 
                 trace_points mode racc liste_points queue
          |'l'-> let racc = (somme (trad tete) acc) in 
                 let liste_points = racc::liste_points in
                 trace_points mode racc liste_points queue
          |'H'-> let racc = ((float_of_string tete),(snd acc)) in 
                 let liste_points = racc::liste_points in
                 trace_points mode racc liste_points queue
          |'V'-> let racc = ((fst acc),(float_of_string tete)) in 
                 let liste_points = racc::liste_points in 
                 trace_points mode racc liste_points queue
          |_-> print_string "Lettre non connue";[] in
            

  let inter_points = trace_points 'l' (0.,0.) [] !interieur in
  let exter_points  = trace_points 'l' (0.,0.) [] !exterieur in
  if (List.length inter_points) != (List.length exter_points) then begin
    print_string "Les deux parties n'ont pas la bonne longueur !!!\n";
    exit 0;
  end;
  milieu := (List.length exter_points);
  for i = 0 to (!milieu-2) do
    trap_tab.(i) <- { ent_int= List.nth inter_points i;
                      ent_ext = List.nth exter_points i ;
                      sort_int = List.nth inter_points (i+1);
                      sort_ext = List.nth exter_points (i+1)};
  done;

  trap_tab.(!milieu-1) <- {ent_int= List.nth inter_points (!milieu-1);
                           ent_ext = List.nth exter_points (!milieu-1) ;
                           sort_int = List.nth inter_points 0;
                           sort_ext = List.nth exter_points 0};
  for i = 0 to (!milieu-1) do print_portion trap_tab.(i) done;
  trap_tab;;
 
 
  

(* ################################################################### *)
(*	                                                                   *)
(*                             MAIN PROCESS                            *)
(*	                                                                   *)
(* ################################################################### *)
	
let main = fun _ ->
	let list_point = conv () in
	print_list list_point;

	let tab_point = trapeze list_point in
	print_tab tab_point;
	print_tab_int tab_point;;

	



