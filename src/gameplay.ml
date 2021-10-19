open Deck
open Hand
open Types
open Player

type input_phrase = string list

type move =
  | Hit
  | Stand

exception NotCommand

(* Potentially read spaces in later implementation *)

(* let start_game = failwith "Unimplemented" *)

let give_cards =
  let og_deck = Deck.create_deck in
  let shuffled = Deck.shuffle og_deck in
  let card_one = Deck.deal_card shuffled in
  let rem_deck = Deck.deal_left shuffled in
  let card_two = Deck.deal_card rem_deck in
  let hand = [ card_one; card_two ] in
  hand

let hit_card hand = Hand.hit (Deck.deal_card hand)

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

let rec use_command str (cards : card list) =
  let command = parse str in
  match command with
  | Hit ->
      let new_cards =
        Deck.add_card
          (deal_card (shuffle (deck_no_hand cards create_deck)))
          cards
      in
      if hand_value new_cards > 21 then
        let () =
          print_endline (hand_string new_cards);
          print_endline "You lose!"
        in
        ()
      else
        let () =
          print_endline (hand_string new_cards);
          print_endline "Do you want to hit (h) or stand (s)?";
          match read_line () with
          | hit -> use_command hit new_cards
        in

        ()
  | Stand ->
      let () =
        print_endline
          ("Your value is: " ^ string_of_int (hand_value cards));
        print_endline "You win"
      in
      ()
(* let make_move move = match move with | [] -> *)
