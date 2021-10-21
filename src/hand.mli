open Types

exception Empty

val empty : card list
(**[empty] is the empty card list*)

val is_empty : card list -> bool
(** [is_empty hand] is [true] when card list [hand] is the empty card
    list and is [false] otherwise.*)

val value_of_card : card -> int
(** [value_of_card card] is the value of card [card], where Ace has
    value 11, other face cards have value 10, and number cards have
    value equal to their number*)

val hit : card -> card list -> card list
(** [hit card hand] is the list of cards containing [card] and all the
    cards in [hand] *)

val hand_total : card list -> int
(** [hand_total hand] is the total value of the cards in card list
    [hand]. Ex:
    [hand_total \[8 of Diamonds, Jack of Hearts, 2 of Diamonds\] is 20]*)

val to_list : card list -> card list
(** [to_list hand] is the list representation of card list [hand]*)

(* val val_to_string : value -> string *)

(* val hand_string : value -> string *)
