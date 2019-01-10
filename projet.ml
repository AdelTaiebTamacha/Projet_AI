open Type;;
open Intersection;;
open Conversion;;


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

(* Contenu du 'a option *)
let contents = fun z -> 
  let Some a = z in
  a;; 
  
(* Définition du coefficient de l'abcisse de la droite angle = f(vitesse) *) 
let coef = fun minvit minang maxvit maxang ->
    let pi = 4.0 *. atan 1.0 in 
    let convert = pi /. 180. in 
    let coefficient = (maxang -. minang )*.convert /. (maxvit -. minvit) in
    let ordonnee = maxang*.convert -. coefficient *. maxvit in
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
    signe(vy)*.(acos(float_of_int(vx-x)/.long));;

(* Angle entre deux vecteurs *)
let anglevect = fun a b c d ->
   let (xa, ya) = a in
   let (xb, yb) = b in
   let ab = distance a b in
   let (xc, yc) = c in
   let (xd, yd) = d in
   let cd = distance c d in 
   acos(float_of_int((xb-xa)*(xd-xc) +(yb-ya)*(yd-yc))/.(ab *. cd));;
   
(* Angle entre deux vecteurs orienté *)
let angleorient = fun a b c d ->
   let (xa, ya) = a in
   let (xb, yb) = b in
   let ab = distance a b in
   let (xc, yc) = c in
   let (xd, yd) = d in
   let cd = distance c d in
   let (xab, yab) = (xb-xa, yb-ya) in
   let (xcd, ycd) = (xd-xc, yd-yc) in 
   let det = xab * ycd - xcd *yab in 
   signe(det)*.acos(float_of_int((xb-xa)*(xd-xc) +(yb-ya)*(yd-yc))/.(ab *. cd));;
   
(* Angle par rapport à l'abcisse *)
let angleabs = fun coord vitesse ->
   let (vx,vy) = vitesse in
   let (x,y) = coord in
   if vy < y then 
      -1.*.anglevect coord vitesse (0,0) (10,0)
   else 
       anglevect coord vitesse (0,0) (10,0);;

(* Savoir si deux segments se croisent *)
let coupent = fun a b c d -> 
  let ab = distance a b in
  let cd = distance c d in
  let ad = distance a d in
  let ac = distance a c in
  let cb = distance c b in
  let angleabcd = angleorient a b c d in 
  let angleabad = angleorient a b a d in
  let angleabac = angleorient a b a c in
  let anglecdcb = angleorient c d c b in
  let anglecdca = angleorient c d c a in
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
    let ang = angleabs coord vitesse in
    let xs = int_of_float ((long +. 4.) *. cos(ang)) + x  in
    let ys = int_of_float ((long +. 4.) *. sin(ang)) + y in
    (xs, ys);;

(* Coordonnée possible si on décélère *) 
let decelere = fun coord vitesse ->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angleabs coord vitesse in
    let xs = int_of_float ((long -. 4.) *. cos ang) + x in
    let ys = int_of_float ((long -. 4.) *. sin ang) + y in
    (xs, ys);;


(* Coordonnée possible si on tourne à gauche *)   
let gauche = fun coord vitesse minvit minang maxvit maxang-> 
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angleabs coord vitesse in
    let (coefficient, ordonnee) = coef minvit minang maxvit maxang in 
    let supang = coefficient *. long +. ordonnee in
    let xs = int_of_float (long *. cos(ang+. supang) ) + x in
    let ys = int_of_float (long *. sin(ang+. supang) )+ y in
    (xs, ys);;

(* Coordonnée possible si on tourne à droite *)
let droite = fun coord vitesse minvit minang maxvit maxang->
    let (x, y) = coord in 
    let long = distance coord vitesse in 
    let ang = angleabs coord vitesse in
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

let res = possibilites (12,96) (12,98) 2. 10. 15. 1.;;

(* Renvoie la position de la vitesse pour le noeud choisit *)
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
   
(* Algorithme A* *) 
(* ATTENTION : il faut rentrer une vitesse initiale qui soit comprise entre la vitesse minimale et maximale, par ailleurs l'angle de la vitesse minimale est supérieur à celui de la vitesse maximale *) 
(* Coordinalg et coordfinald correspondent à les coordonnées du segment de fin *) 
let aetoile = fun noeuddepart coordfinalg coordfinald vitesseinitiale minvit minang maxvit maxang circuit->
   (*Initialisation des listes : ouverte et fermée, et variables *)
   let listeouverte = ref [] in
   let listefermee = ref [] in 
   let noeudcourant = ref {cout_g = 0.; cout_h = distance noeuddepart.coord  coordfinalg; cout_f = distance  noeuddepart.coord  coordfinalg; parent = Some noeuddepart; coord = noeuddepart.coord; vitesse = vitesseinitiale} in
   let noeudobserve = ref {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = Some noeuddepart; coord = noeuddepart.coord; vitesse = (0,0)} in
   let indexcourant = ref 0 in
   let solution = ref [] in 
   let portion = ref circuit.(0) in 
   let indexportion = ref 0 in
   (*Initialisation du noeud courant et ajout à la liste ouverte *)
   listeouverte := List.append !listeouverte [Noeud !noeudcourant];
   Printf.printf "taille liste ouverte :  %d\n" (List.length !listeouverte);
   (*Boucle jusqu'à trouver le noeud final*)
   while !listeouverte <> []  && not (coupent !noeudcourant.coord !noeudcourant.vitesse coordfinalg coordfinald) do 
      (*Initialisation du noeud courant au noeud du début*)
      noeudcourant := valeur !listeouverte 0;
      indexcourant := 0;
      for k=0 to (List.length !listeouverte -1)do 
          noeudobserve := valeur !listeouverte k;
          if ! noeudobserve.cout_f < ! noeudcourant.cout_f then 
              noeudcourant := ! noeudobserve;
              indexcourant := k 
      done;
      Printf.printf "Noeud courant : %d,%d\n" (fst !noeudcourant.coord) (snd !noeudcourant.coord);
      (*Retire le noeud courant de la liste ouverte et l'ajoute à la liste fermée*)
      listeouverte := remove !noeudcourant !listeouverte;
      listefermee := List.append !listefermee [Noeud !noeudcourant];
      (*Générer les enfants*)
      let enfants = ref [] in
      let poss = possibilites !noeudcourant.coord !noeudcourant.vitesse minvit minang maxvit maxang in
      for i = 0 to 4 do
        Printf.printf "poss : %d,%d\n" (fst poss.(i)) (snd poss.(i));
      done;
      for k=0 to 4 do 
        (*Avoir la position du noeud*)
        let pos = poss.(k) in 
        (* Savoir la parallélogramme dans lequel on est *) 
        let entint = !portion.ent_int_int in
        let entext = !portion.ent_ext_int in
        let sortint = !portion.sort_int_int in
        let sortext = !portion.sort_ext_int in
        (*Si il va dans le parallélogramme d'après, on change la valeur de paral *)
        if coupent sortint sortext !noeudcourant.coord pos then  
          (indexportion := !indexportion + 1;
           portion := circuit.(!indexportion)); 
        (*Sors pas du terrain et ne fait pas demi-tour*)
        if (not(coupent entext sortext !noeudcourant.coord pos))|| not(coupent entint sortint !noeudcourant.coord pos)|| not(coupent entint entext !noeudcourant.coord pos) then
            (* Vitesse *)
            let vit = choixvitesse pos ! noeudcourant.coord in 
            let speed = distance vit pos in 
            Printf.printf "Vitesse : %d,%d\n" (fst vit) (snd vit);
            (*Creer un nouveau noeud si la vitesse ne dépasse pas la vitesse max *)
            let nouveaunoeud =  ref {cout_g =0.; cout_h=0.; cout_f=0.; parent = Some (! noeudcourant); coord= pos; vitesse = vit}in
            (* Si la vitesse est valable on l'ajoute à la liste des enfants *)
            if speed <= maxvit && speed >= minvit then 
                  enfants := List.append !enfants [Noeud ! nouveaunoeud]
      done;
      for k=0 to (List.length !enfants - 1) do
        Printf.printf "Noeud courant truc : %d,%d\n" (fst !noeudcourant.coord) (snd !noeudcourant.coord); 
        let enfant = valeur !enfants k in
        Printf.printf "Enfants : %d\n" (List.length !enfants);
          (* enfant est dans la liste fermée*)
        if not (appartient  enfant !listefermee) then 
          (* créer les valeurs de f. g. et h.*)
          (enfant.cout_g <- ! noeudcourant.cout_g +. 1.;
           enfant.cout_h <- distance  enfant.coord  coordfinalg;
           enfant.cout_f <-  enfant.cout_g +.  enfant.cout_h;
             (* enfant est déjà dans la liste ouverte *)
           let continue = ref 0 in 
           for k=0 to (List.length !listeouverte) -1 do 
             let noeudouvert = valeur !listeouverte k in 
             if enfant.coord = noeudouvert.coord && enfant.cout_g > noeudouvert.cout_g then
               continue := 1 
           done;
           if !continue = 0 then
             listeouverte := List.append !listeouverte [Noeud  enfant];)
      (* ajouter enfant à liste ouverte*)
      done;
   done;
   (*Fin trouvé*)
   let path = ref [!noeudcourant.vitesse] in
   let courant = ref  (Some (! noeudcourant)) in 
   while !courant <>  None do 
        path := List.append !path [(contents(!courant)).coord];
        courant := ((contents(!courant)).parent)
   done;
   solution := List.rev !path;
   !solution;;


(*

aetoile noeuddepart noeudfinal vitesseinitiale minvit minang maxvit maxang
type defnoeud = {mutable cout_g : float; mutable cout_h : float; mutable cout_f : float; mutable parent : defnoeud; mutable coord : int*int; mutable vitesse : int*int};;

*)
(*
let noeuddepart = {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = None; coord = (10,10); vitesse = (0,0)};;
let noeudfinal = {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = None; coord = (0,200); vitesse = (0,0)};;
let solution = aetoile noeuddepart (90,100) (100,90) (15,15) 5. 40. 80. 10. ;;
solution;;

*)

let search = fun circuit ->
	
	let coord_depart = middle_start circuit.(0) in
	let noeuddepart = {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = None; coord = coord_depart; vitesse = (0,0)} in
	let noeudfinal = {cout_g = 0.; cout_h = 0.; cout_f = 0.; parent = None; coord = coord_depart; vitesse = (0,0)} in
	let solution = aetoile noeuddepart (90,100) (100,90) (15,15) 5. 40. 80. 10. circuit in
	solution;;


