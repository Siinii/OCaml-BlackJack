open Game.Deck
open Game.Hand
open Game.Gameplay
open Game.Player
open Game.Types
open Game.Gui
open Graphics

(* GUI Section *)
let card_pic =
  [ (160, 150); (240, 150); (240, 210); (160, 210) ] |> Array.of_list

let open_gui = open_graph ""

let draw_card card =
  set_color Graphics.magenta;
  fill_poly card_pic

let rec draw_hand hand =
  match hand with
  | [] -> ()
  | h :: t ->
      draw_card h;
      draw_hand t

(*GUI Section Done*)
let play_game f =
  print_endline ("Your hand is: " ^ hand_string give_init_cards);
  draw_hand [ 1; 2 ];
  ANSITerminal.print_string [ ANSITerminal.white ]
    "Do you want to hit (h) or stand (s)?";
  match read_line () with
  | cmnd -> use_command cmnd give_init_cards

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
