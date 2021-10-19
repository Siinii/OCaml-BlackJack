open Game.Deck
open Game.Hand
open Game.Gameplay
open Game.Player
open Game.Types

let play_game f =
  let play_hand = give_cards;
  let player (Game.Player.player)= {name = f; hand = play_hand}
  print_endline (hand_string give_cards);
  ANSITerminal.print_string [ ANSITerminal.white ]
    "Do you want to hit (h) or stand (s)?";
  let command = match read_line () with
  | x -> x in hit player
      print_endline "You win"

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
