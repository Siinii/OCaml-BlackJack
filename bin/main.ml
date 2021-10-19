open Game.Deck
open Game.Hand
open Game.Gameplay
open Game.Player
open Game.Types

let play_game f =
  print_endline ("Your hand is: " ^ hand_string give_cards);
  ANSITerminal.print_string [ ANSITerminal.white ]
    "Do you want to hit (h) or stand (s)?";
  match read_line () with
  | hit -> use_command hit give_cards

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.white ]
    "\n\nWelcome to Blackjack!\n";
  print_endline "Please enter your name.\n";
  print_string "> ";

  match read_line () with
  | name -> play_game name

(* Execute the game engine. *)
let () = main ()
