
(** Type definition of the Conversion Module*)

type portion_float = {
  ent_int : float * float;
  ent_ext : float * float;
  sort_int : float * float;
  sort_ext : float * float;
}

(**
Tuple of the portion coordinates floating value
ent_int - entrée interieure
ent_ext - entrée exterieure
sort_int - sortie interieure
sort_ext - sortie exterieure
*)

type portion_int = {
  ent_int_int : int * int;
  ent_ext_int : int * int;
  sort_int_int : int * int;
  sort_ext_int : int * int;
}
(**
Tuple of the portion coordinates integer value
ent_int - entrée interieure
ent_ext - entrée exterieure
sort_int - sortie interieure
sort_ext - sortie exterieure
*)
