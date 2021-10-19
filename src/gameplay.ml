open Deck
open Hand
open Types

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

(* let make_move move = match move with | [] -> *)
