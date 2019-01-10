(* Calcul la puissance d'un entier *)
let rec puissance x n = match n with
| 0 -> 1
| _ -> x * (puissance x (n-1))
;;


(* Calcul la distance entre deux coordonnées *)
let distance = fun coord1 coord2 ->
    let (x1, y1) = coord1 in
    let (x2, y2) = coord2 in 
    sqrt ( float_of_int(puissance (x2-x1) 2 + puissance (y2-y1) 2 ));;

	
(* Définition de la vitesse minimale *)
let vitessemin = fun x ->
    x;;

	
(* Définition de la vitesse maximale *)
let vitessemax = fun x ->
    x;;
(* Contenu du 'a option *)
let contents = fun z -> 
  let Some a = z in
  a;; 
(* Définition du coefficient de l'abcisse de la droite angle = f(vitesse) *) 
let coef = fun minvit minang maxvit maxang ->
    let pi = 4.0 *. atan 1.0 in 
    let convert = pi /. 180. in 
    let coefficient = (minang -. maxang )*.convert /. (maxvit -. minvit) in
    let ordonnee = minang*.convert -. coefficient *. maxvit in
    (coefficient, ordonnee);;
 
 
(* Savoir le signe *)
let signe = fun x ->
    if x >=0 then
       1.
    else 
       -1.;;

(* Angle par rapport à l'axe des abcisses d'un vecteur *)
let angle = fun coord vitesse ->
    let (x, y) = coord in
    let (vx, vy) = vitesse in 
    let long = distance coord vitesse in
    signe(y)*.(acos(float_of_int(vx-x)/.long));;

(* Angle entre deux vecteurs *)
let anglevect = fun a b c d ->
   let (xa, ya) = a in
   let (xb, yb) = b in
   let ab = distance a b in
   let (xc, yc) = c in
   let (xd, yd) = d in
   let cd = distance c d in 
   acos(float_of_int((xb-xa)*(xd-xc) +(yb-ya)*(yd-yc))/.(ab *. cd));;
   
let coupent = fun a b c d -> 
  let ab = distance a b in
  let cd = distance c d in
  let ad = distance a d in
  let ac = distance a c in
  let cb = distance c b in
  let angleabcd = anglevect a b c d in 
  let angleabad = anglevect a b a d in
  let angleabac = anglevect a b a c in
  let anglecdcb = anglevect c d c b in
  let anglecdca = anglevect c d c a in
  let prodvectabcd = ab*.cd*.sin(angleabcd) in
  let prodvectabad = ab*.ad*.sin(angleabad) in
  let prodvectabac = ab*.ac*.sin(angleabac) in
  let prodvectcdcb = cd*.cb*.sin(anglecdcb) in
  let prodvectcdca = cd*.ac*.sin(anglecdca) in
  if prodvectabcd <> 0. && prodvectabad*.prodvectabac <= 0. && prodvectcdcb*.prodvectcdca <= 0. then 
          true
  else 
          false;;
	
(* Coordonnée possible si on continue *)
let continue = fun coord vitesse ->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let xs = int_of_float ((long) *. cos(ang)) + x  in
    let ys = int_of_float ((long) *. sin (ang)) + y in
    (xs, ys);;
	
	
(* Coordonnée possible si on accélère *)
let accelere = fun coord vitesse ->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let xs = int_of_float ((long +. 4.) *. cos(ang)) + x  in
    let ys = int_of_float ((long +. 4.) *. sin (ang)) + y in
    (xs, ys);;


(* Coordonnée possible si on décélère *) 
let decelere = fun coord vitesse ->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let xs = int_of_float ((long -. 4.) *. cos ang) + x in
    let ys = int_of_float ((long -. 4.) *. sin ang) + y in
    (xs, ys);;


(* Coordonnée possible si on tourne à gauche *)   
let gauche = fun coord vitesse minvit minang maxvit maxang-> 
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let (coefficient, ordonnee) = coef minvit minang maxvit maxang in 
    let supang = coefficient *. long +. ordonnee in
    let xs = int_of_float (long *. cos(ang+. supang) ) + x in
    let ys = int_of_float (long *. sin(ang+. supang) )+ y in
    (xs, ys);;


(* Coordonnée possible si on tourne à droite *)
let droite = fun coord vitesse minvit minang maxvit maxang->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angle coord vitesse in
    let (coefficient, ordonnee) = coef minvit minang maxvit maxang in 
    let supang = coefficient *. long +. ordonnee in
    let xs = int_of_float ( long *. cos(ang -. supang) )+ x in 
    let ys = int_of_float ( long *. sin(ang -. supang) )+ y in
    (xs, ys);;


(* Possibilités dans un tableau *)   
let possibilites = fun coord vitesse minvit minang maxvit maxang -> 
    let suivante = [|(0,0);(0,0);(0,0);(0,0);(0,0)|] in 
    let (vx, vy) = vitesse in 
    let (ax, ay) = accelere coord vitesse in 
    let (dx, dy) = decelere coord vitesse in
    let (gx, gy) = gauche coord vitesse minvit minang maxvit maxang in
    let (drx, dry) = droite coord vitesse minvit minang maxvit maxang in
    suivante.(0) <- (vx, vy);
    suivante.(1) <- (ax, ay);
    suivante.(2) <- (dx, dy);
    suivante.(3) <- (gx, gy);
    suivante.(4) <- (drx, dry);
    suivante;;


let choixvitesse = fun vitesse position ->
	let (vx,vy) = vitesse in
	let (x,y) = position in
	let vectvitesse = (2*vx-x, 2*vy-y) in
	vectvitesse;;


(* Définition d'un type noeud *)
type defnoeud = {mutable cout_g : float; mutable cout_h : float; mutable cout_f : float; mutable parent : defnoeud option; mutable coord : int*int; mutable vitesse : int*int} ;;
type noeud = Noeud of defnoeud;;


(* Retirer un noeud d'une liste de noeud *)
let rec remove = fun noeud liste -> match liste with 
    [] -> failwith "Nan mais franchement tu déconnes, c'est pas comme ça qu'on fait les choses" 
   |Noeud h::q-> if h = noeud then 
             q
          else 
             Noeud h::(remove noeud q);;
  
(* Avoir la valeur du noeud qui se trouve à l'index 'num' *)           
let rec valeur = fun liste num -> match liste with 
    []-> failwith "y'a pas moyen djadja"
   |Noeud h::q-> if num = 0 then
               h
          else 
               valeur q (num-1);;
               
(* Valeur dans liste *)
let rec appartient = fun noeud liste -> match liste with
   []-> false
   |Noeud h::q -> if h== noeud then
                    true
                 else 
                    appartient noeud q;;                 

               
let essai = fun a ->
   let liste = ref [] in
   while !liste = [] do
      liste := List.append !liste [a]
   done;
   !liste;;

 
let print_noeud = fun noeud ->
	let (x,y) = noeud.coord in 
	Printf.printf "(%d,%d)" x y;;
   
(* Algorithme A* *) 
let aetoile = fun noeuddepart noeudfinal vitesseinitiale minvit minang maxvit maxang ->
   (*Initialisation des listes : ouverte et fermée, et variables *)
   let listeouverte = ref [] in
   let listefermee = ref [] in 
   let noeudcourant = ref {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = Some noeuddepart; coord = noeuddepart.coord; vitesse = (0,0)} in
   let noeudobserve = ref {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = Some noeuddepart; coord = noeuddepart.coord; vitesse = (0,0)} in
   let indexcourant = ref 0 in
   let solution = ref [] in 
   let debugcounter = ref 0 in
   
   
   (*Initialisation du noeud courant et ajout à la liste ouverte *)
   let noeuddebut = {cout_g = 0.; cout_h = distance noeuddepart.coord  noeudfinal.coord; cout_f = distance  noeuddepart.coord  noeudfinal.coord; parent = Some noeuddepart; coord = noeuddepart.coord; vitesse = vitesseinitiale} in 
   listeouverte := List.append !listeouverte [Noeud  noeuddebut];
   (*Boucle jusqu'à trouver le noeud final*)
   while !listeouverte <> [] && ! noeudcourant.coord <>  noeudfinal.coord do 
		
		Printf.printf "Debug cycle : %d, \n" !debugcounter;
		debugcounter := !debugcounter + 1;
		Printf.printf "List listeouverte = ";
		List.iter print_noeud listeouverte;
		Printf.printf " Fin liste ouverte \n"
		
      (*Initialisation du noeud courant au noeud du début*)
      noeudcourant := valeur !listeouverte 0;
      indexcourant := 0;
      for k=0 to List.length !listeouverte - 1 do 
          noeudobserve := valeur !listeouverte k;
          if ! noeudobserve.cout_f < ! noeudcourant.cout_f then 
              noeudcourant := ! noeudobserve;
              indexcourant := k 
      done;
      (*Retire le noeud courant de la liste ouverte et l'ajoute à la liste fermée*)
      listeouverte := remove ! noeudcourant !listeouverte;
      listefermee := List.append !listefermee [Noeud ! noeudcourant];
      (*Trouver la fin*)
      if ! noeudcourant.coord ==  noeudfinal.coord then
         let path = ref [] in
         let courant = ref  (! noeudcourant) in 
         while ! courant <>  noeuddepart do 
              path := List.append !path [!courant.coord];
              courant :=  contents (!courant.parent)
         done;
         solution := List.rev !path;
      solution := [(1,1)];
      (*Générer les enfants*)
      let enfants = ref [] in
      let poss = possibilites ! noeudcourant.coord ! noeudcourant.vitesse minvit minang maxvit maxang in
      for k=0 to 4 do 
         (*Avoir la position du noeud*)
         let pos = poss.(k) in 
         (* Savoir la parallélogramme dans lequel on est *) 
         (*let (entint,entext,sortint,sortext) = parallelogramme in 
         (*Sors pas du terrain et ne fait pas demi-tour*)
         if not(coupent entext sortext noeudcourant.coord pos)|| not(coupent entint sortint noeudcourant.coord pos)|| not(coupent entint entext noeucourant.coord pos)   then *)
              (* Vitesse *)
              let vit = choixvitesse ! noeudcourant.vitesse ! noeudcourant.coord in 
              (*Creer un nouveau noeud*)
              let nouveaunoeud =  ref {cout_g =0.; cout_h=0.; cout_f=0.; parent = Some (! noeudcourant); coord= pos; vitesse = vit}in
              (*append*)
              enfants := List.append !enfants [Noeud ! nouveaunoeud]
      done;
      for k=0 to List.length !enfants do
          let enfant = valeur !enfants k in
          (* enfant est dans la liste fermée*)
          if not (appartient  enfant !listefermee) then 
             (* créer les valeurs de f. g. et h.*)
              enfant.cout_g <- ! noeudcourant.cout_g +. 1.;
              enfant.cout_h <- distance  enfant.coord  noeudfinal.coord;
              enfant.cout_f <-  enfant.cout_g +.  enfant.cout_f;
             (* enfant est déjà dans la liste ouverte *)
             let indic = ref 1 in 
             let valnoeud = ref (valeur !listeouverte 0) in
             while  enfant <> ! valnoeud &&  enfant.cout_g <= ! valnoeud.cout_g && !indic <> List.length !listeouverte do 
                 valnoeud := valeur !listeouverte !indic;
                 indic := !indic +1;
             done;
             (* ajouter enfant à liste ouverte*)
             listeouverte := List.append !listeouverte [Noeud  enfant]
      done;
   done;
   !solution;;


(*

aetoile noeuddepart noeudfinal vitesseinitiale minvit minang maxvit maxang
type defnoeud = {mutable cout_g : float; mutable cout_h : float; mutable cout_f : float; mutable parent : defnoeud; mutable coord : int*int; mutable vitesse : int*int};;

*)
let noeuddepart = {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = None; coord = (0,0); vitesse = (0,0)};;
let noeudfinal = {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = None; coord = (100,100); vitesse = (0,0)};;
let solution = aetoile noeuddepart noeudfinal (10,10) 10. 5. 15. 1.;;

Printf.printf "Solution \n";;
Printf.printf "Nombre d'element : %d \n" (List.length solution);;
List.iter (fun (x,y) -> Printf.printf "Node : %d %d" x y) solution;;

  
