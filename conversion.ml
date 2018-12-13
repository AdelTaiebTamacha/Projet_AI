
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
let f = fun a -> Printf.printf " %s \n" a;; 
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
            "d=\"m"::queue ->coordfun queue; 
          |tete::queue -> recherche queue;
          |"</svg>" -> coord_list ;
          |_ -> [];
      let coord_list =(recherche lignesep)::coord_list in
      conv_rec coord_list; 
    with End_of_file -> close_in fic
  in conv_rec [];;

conv 1;;


  
