open Game.Deck
open Game.Hand
open Game.Gameplay
open Game.Player
open Game.Types

let play_game f =
  let () = print_endline (hand_string give_cards) in
  ()

(* REMOVE LATER *)
let val_to_string value =
  match value with
  | Ace -> "Ace"
  | King -> "King"
  | Queen -> "Queen"
  | Jack -> "Jack"
  | Number x -> string_of_int x

let suit_to_string suit =
  match suit with
  | Hearts -> "Hearts"
  | Diamonds -> "Diamonds"
  | Spades -> "Spades"
  | Clubs -> "Clubs"

let card_to_string card =
  match card with
  | { valu; suit } -> val_to_string valu ^ "of" ^ suit_to_string suit

let rec hand_string (hand : card list) =
  match hand with
  | [] -> ""
  | h :: t -> card_to_string h ^ " , " ^ hand_string t

(* REMOVE LATER *)

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
