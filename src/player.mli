open Types
open Deck
open Hand

type player = {
  name : string;
  hand : card list;
}

type state =
  | PLAYING
  | BUSTED
  | STANDING

val currentState : state

val card1 : card

val nth_card : card list -> int -> int -> card

val card2 : card

val initHand : card list

val value_of_card_player : card -> int

val player_value : card list -> int

val is_busted : card list -> bool

val hit : card list -> card list
