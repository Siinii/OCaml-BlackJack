(** [play_game f] starts the adventure in file [f]. *)
let play_game f = failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.white ]
    "\n\nWelcome to Blackjack!\n";
  print_endline "Please enter your name.\n";
  print_string "> ";
  match read_line () with
  | text -> play_game text

(* Execute the game engine. *)
let () = main ()
