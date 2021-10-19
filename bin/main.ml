open Game.Deck
open Game.Hand
open Game.Gameplay
open Game.Player
open Game.Types

let play_game f =
  let () = print_endline (hand_string give_cards) in
  ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.white ]
    "\n\nWelcome to Blackjack!\n";
  print_endline "Please enter your name.\n";
  print_string "> ";

  match read_line () with
  | name ->
      let () = print_int (Random.bits ()) in
      play_game name

(* Execute the game engine. *)
let () = main ()
