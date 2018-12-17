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
let f = fun a -> Printf.printf " %s |" a;; 
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

let trace = fun list -> 
  let milieu = (List.length list)/2 in
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
  print_string "\n";



let a = conv 1 in
List.iter f a;
print_string "\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n";
trace a;;

let trapeze = { ent_int=(1,2);
                ent_ext = (4,5);
                sort_int=(7,8);
              sort_ext=(2,78)} in
let (x,y) = trapeze.ent_int in
Printf.printf "%d,%d" x y;;  

