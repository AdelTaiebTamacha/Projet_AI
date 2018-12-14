type point =
  | EntInt of int*int
  | EntExt of int*int
  | SortInt of int*int
  | SortExt of int*int;;

type trapeze =
  | Portion of point*point*point*point;;
