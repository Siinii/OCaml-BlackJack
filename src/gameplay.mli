open Deck
open Hand
open Types
open Player

type input_phrase = string list

val init_state : game_state

val new_state : game_state -> string -> unit

type move =
  | Hit
  | Stand
  | Bet of int
  | PlaceHolder

exception NotCommand

val shuffled : card list
(** [shuffled] is a shuffled 52-card deck*)

val rem_deck_nth : int -> card list
(** [rem_deck_nth] is a remaining deck after the top n cards have been
    removed from a shuffled 52-card deck*)

val card_n : int -> card
(** [card_n n] is the nth-from-the-top card in a shuffled 52-card deck*)

val give_init_cards : card list
(**[give_init_cards] is the list of two random cards that are dealt to a
   player from a shuffled deck at the start of a round*)

val ai_init_hand : card list
(** [ai_init_hand] is the list of two random cards that are dealt to the
    ai player (after the actual player is dealt his initital hand) from
    a shuffle deck*)

(* val ai_dealer_final_hand : card list -> card list (**
   [ai_dealer_final_hand hand] is the final hand of the dealer, with
   total value >=17, when given initial hand [hand] *)

   val use_command : string -> card list -> unit (** [use_command str
   hand] parses the string [str] and either hits or stands onto the hand
   [hand] depending on the string given. Raises [NotCommand] if an
   invalid command string is passed in.*) *)

val hand_string : card list -> string
(** [hand_string hand] is the string representation of the card list
    [hand]*)
