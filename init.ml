open Graphics;;

let draw_init = fun _ ->
	
	let x = 10 in
	let delta = (-15) in
	let y = ref 350 in
	moveto x !y;
	draw_string "                    ___..................____";
	y := !y + delta;
	moveto x !y;
	draw_string "           _..--''~_______   _____...----~~~\\\\";
	y := !y + delta;
	moveto x !y;
	draw_string "       __.'    .-'~       \\\\~      [_`.7     \\\\";
	y := !y + delta;
	moveto x !y;
	draw_string " .---~~      .'            \\\\           __..--\\\\_";
	y := !y + delta;
	moveto x !y;
	draw_string "/             `-._          \\\\   _...~~~_..---~  ~~~~~~~~~~~~--.._";
	y := !y + delta;
	moveto x !y;
	draw_string "\\              /  ~~~~~~----_\\`-~_-~~__          ~~~~~~~---.._    ~--.__";
	y := !y + delta;
	moveto x !y;
	draw_string " \\     _       |:=:=            |   ~--___--------...__          `-   _.--\"\"\"|\"";
	y := !y + delta;
	moveto x !y;
	draw_string "  \\ __/.-._\\   |              |            ~~~~--.  `-._ ___...--~~~_.'|_Y |";
	y := !y + delta;
	moveto x !y;
	draw_string "   `--'|/~_\\\\  |              |     _____           _.~~~__..--~~_.-~~~.-~/";
	y := !y + delta;
	moveto x !y;
	draw_string "     | ||| |\\\\_|__            |.../.----.._.        | Y |__...--~~_.-~  _/";
	y := !y + delta;
	moveto x !y;
	draw_string "      ~\\\\\\ || ~|..__---____   |||||  .'~-. \\\\       |_..-----~~~~   _.~~";
	y := !y + delta;
	moveto x !y;
	draw_string "        \\`-'/ /     ~~~----...|'''|  |/\"_\"\\ \\\\   |~~'           __.~";
	y := !y + delta;
	moveto x !y;
	draw_string "         `~~~'                 ~~-:  ||| ~| |\\\\  |        __..~~";
	y := !y + delta;
	moveto x !y;
	draw_string "                                   ~~|||  | | \\\\/  _.---~~";
	y := !y + delta;
	moveto x !y;
	draw_string "                                     \\\\\\  //  | ~~~";
	y := !y + delta;
	moveto x !y;
	draw_string "                                      \\`-'/  / dp";
	y := !y + delta;
	moveto x !y;
	draw_string "                                       `~~~~'";
	y := !y + delta;
	moveto x !y;;