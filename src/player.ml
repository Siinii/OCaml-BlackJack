open Types
open Deck
open Hand

exception Empty

type player = {
  name : string;
  hand : card list;
}

type state =
  | PLAYING
  | BUSTED
  | STANDING

let currentState = PLAYING

let card1 = Deck.deal_card Deck.create_deck

let rec nth_card deck n acc =
  match deck with
  | [] -> raise Empty
  | h :: t -> if acc + 1 = n then (h : card) else nth_card t n (acc + 1)

let card2 = nth_card Deck.create_deck 1 0

let initHand = [ card1 ] @ [ card2 ]

let value_of_card_player (c : card) =
  match c with
  | { valu = Ace; _ } -> 11
  | { valu = Number x; _ } -> x
  | { valu = _; _ } -> 10

let rec player_value hand =
  match hand with
  | [] -> 0
  | h :: t -> value_of_card_player h + player_value t

let is_busted (hand : card list) =
  if player_value hand > 21 then true else false

let rec hit hd =
  match hd with
  | [] -> if is_busted hd = false then hd else raise Empty
  | h :: t ->
      if is_busted hd = false && currentState = PLAYING then
        Hand.hit h hd
      else hit []
