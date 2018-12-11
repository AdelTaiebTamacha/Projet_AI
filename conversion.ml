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

let coordfun = fun list ->
  let rec coord_rec = fun list coor ->
    match list with
        "z\""::queue ->
          coor;
          print_string("fin");
      |tete::queue ->
        coor = List.append coor [tete];
        print_string(tete);
        coord_rec queue coor;
      |_ ->
        ["pb"];
        print_string("pb") in
  coord_rec list [];;

let conv = fun a ->
  let fic = open_in "dessin.svg" in
  let rec conv_rec() =
    try
      let ligne = input_line fic in
      let lignesep = split_on_char ' ' ligne in
      let rec recherche = fun lsep ->
        match lsep with
            "d=\"m"::queue -> coordfun(queue);
          |tete::queue -> print_string("a"); recherche queue;
          |[] -> print_string("fin") in
      recherche lignesep;
      print_newline();
      conv_rec();
    with End_of_file -> close_in fic
  in conv_rec();;

conv 1;;


  
