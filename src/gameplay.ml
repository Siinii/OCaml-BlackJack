open Deck
open Hand
open Types
open Player
open Gui
open Graphics

type input_phrase = string list

type move =
  | Hit
  | Stand

exception NotCommand

(* Potentially read spaces in later implementation *)

(* let start_game = failwith "Unimplemented" *)

let og_deck = Deck.create_deck

let shuffled = Deck.shuffle og_deck

let card_one = Deck.deal_card shuffled

let rem_deck = Deck.deal_left shuffled

let card_two = Deck.deal_card rem_deck

let rem_deck_second = Deck.deal_left rem_deck

let card_three = Deck.deal_card rem_deck_second

let rem_deck_third = Deck.deal_left rem_deck_second

let card_four = Deck.deal_card rem_deck_third

let rec n_times f n x = if n > 0 then n_times f (n - 1) (f x) else x

let rem_deck_nth n = n_times Deck.deal_left n shuffled

let card_n n = Deck.deal_card (rem_deck_nth (n - 1))

let give_init_cards =
  let hand = [ card_n 1; card_n 2 ] in
  hand

let ai_init_hand = [ card_n 3; card_n 4 ]

(**What is this supposed to do??*)
(*let hit_card (hand : card list) : card list = Hand.hit (Deck.deal_card
  hand) *)

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
  | Hearts -> " Hearts"
  | Diamonds -> " Diamonds"
  | Spades -> " Spades"
  | Clubs -> " Clubs"

let card_to_string card =
  match card with
  | { valu; suit } -> val_to_string valu ^ " of" ^ suit_to_string suit

let rec hand_string (hand : card list) =
  match hand with
  | [] -> ""
  | [ h ] -> card_to_string h
  | h :: t -> card_to_string h ^ " , " ^ hand_string t

(* REMOVE LATER *)
let parse str =
  match str with
  | "hit" -> Hit
  | "h" -> Hit
  | "s" -> Stand
  | "stand" -> Stand
  | _ -> raise NotCommand

let rec ai_dealer_final_hand cards =
  match cards with
  | [] -> failwith "Unimplemented"
  | h :: t ->
      let hit_hand =
        Deck.add_card
          (deal_card (shuffle (deck_no_hand cards create_deck)))
          cards
      in
      if Hand.hand_total cards < 17 then ai_dealer_final_hand hit_hand
      else cards

let rec use_command str (cards : card list) =
  let command = parse str in
  match command with
  | Hit ->
      let new_cards =
        Deck.add_card
          (deal_card (shuffle (deck_no_hand cards create_deck)))
          cards
      in
      if hand_total new_cards > 21 then
        let () =
          print_endline (hand_string new_cards);
          print_endline "You lose!"
        in
        ()
      else
        let () =
          print_endline ("Your hand is: " ^ hand_string new_cards);
          print_endline
            ("The Dealer's first card is: "
            ^ card_to_string (deal_card ai_init_hand));
          print_endline "Do you want to hit (h) or stand (s)?";
          match read_line () with
          | hit -> use_command hit new_cards
        in

        ()
  | Stand ->
      let dealer_hand = ai_dealer_final_hand ai_init_hand in
      let dealer_val = hand_total dealer_hand in
      let () =
        print_endline
          ("Your total is: " ^ string_of_int (hand_total cards));
        print_endline
          ("The Dealer's hand is: " ^ hand_string dealer_hand);
        print_endline ("Dealer's total is: " ^ string_of_int dealer_val);
        if dealer_val <= 21 && dealer_val > hand_total cards then
          print_endline "You lose"
        else print_endline "You win"
      in
      ()
(* let make_move move = match move with | [] -> *)
