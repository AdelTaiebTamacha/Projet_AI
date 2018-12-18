open Type;;
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
let f = fun a -> Printf.printf " %f |" a;; 
let print_tuple = fun (x,y) -> Printf.printf "%f,%f\n" x y;;  
let somme = fun (x,y) (v,w) -> (x+.v,y+.w);;
let print_portion = fun port ->
  let (a,b) = port.ent_int in
  let (c,d) = port.ent_ext in
  let (e,f) = port.sort_int in 
  let (g,h) = port.sort_ext in
  Printf.printf "(%f,%f) (%f,%f) (%f,%f) (%f,%f)\n" a b c d e f g h;;




let coordfun = fun list ->
  let rec coord_rec = fun list coor ->
    match list with
        "z\""::queue ->
          coor;
      |tete::queue ->
        let coor = tete::coor in
        coord_rec queue coor;
      |_ ->
        ["fin"] in
  coord_rec list [];;

let conv = fun a ->
  let fic = open_in "dessin.svg" in
  let rec conv_rec coord_list =
    try
      let ligne = input_line fic in
      let lignesep = split_on_char ' ' ligne in
      let rec recherche = fun lsep ->
        match lsep with
            "d=\"m"::queue ->List.rev (coordfun queue); 
          |"</svg>"::_ -> [];
          |tete::queue -> recherche queue;
          |_ -> [] in 
      conv_rec (recherche lignesep)@coord_list;  
    with End_of_file -> close_in fic;coord_list in
  conv_rec [];;

let trapeze = fun list -> 
  let milieu = (List.length list)/2 in
  let trap_tab = Array.make milieu  {ent_int= (0.,0.);
                                     ent_ext = (0.,0.);
                                     sort_int = (0.,0.);
                                     sort_ext = (0.,0.)} in
  let interieur = ref [] in
  let exterieur = ref [] in 
  for i = 0 to (milieu-1) do
    exterieur := (List.nth list i)::(!exterieur);
  done;
  for i = milieu to (List.length list - 1) do
    interieur := (List.nth list  i)::(!interieur);
  done;
  interieur := List.rev !interieur;
  exterieur := List.rev !exterieur;
  let rec trace_points = fun acc liste_points liste ->
    match liste  with
        tete::queue -> let racc = (somme (trad tete) acc) in 
                       let liste_points = racc::liste_points in
                       trace_points racc liste_points queue;
      |[]-> List.rev liste_points in
  let inter_points = trace_points (0.,0.) [] !interieur in
  let exter_points  = trace_points (0.,0.) [] !exterieur in
  for i = 0 to (milieu-2) do
    trap_tab.(i) <- { ent_int= List.nth inter_points i;
                          ent_ext = List.nth exter_points i ;
                          sort_int = List.nth inter_points (i+1);
                          sort_ext = List.nth exter_points (i+1)};
  done;
  trap_tab.(milieu-1) <- {ent_int= List.nth inter_points (milieu-1);
                          ent_ext = List.nth exter_points (milieu-1) ;
                          sort_int = List.nth inter_points 0;
                          sort_ext = List.nth exter_points 0};
 (* for i = 0 to (milieu-1) do print_portion trap_tab.(i) done;*)
  trap_tab;;

let a = conv 1;;
(*trapeze a;;*)


