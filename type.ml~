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
    
let a = "Hello World";;
let b = split_on_char ' ' a;;
let f = fun a ->
  Printf.printf "%s/" a;;
List.iter f b;;
