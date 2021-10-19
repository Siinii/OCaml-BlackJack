open Deck
include Hand
open Types

type input_phrase = string list

type move =
  | Hit
  | Stand

exception NotCommand

(* Potentially read spaces in later implementation *)

let start_game = failwith "Unimplemented"

let give_cards =
  let og_deck = Deck.create_deck in
  let card_one = Deck.deal_card og_deck in
  let rem_deck = Deck.deal_left og_deck in
  let card_two = Deck.deal_card rem_deck in
  let hand = [ card_one; card_two ] in
  hand

let parse str =
  match str with
  | "hit" -> Hit
  | "h" -> Hit
  | "s" -> Stand
  | "stand" -> Stand
  | _ -> raise NotCommand

(* let make_move move = match move with | [] -> *)
