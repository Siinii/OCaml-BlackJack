open Types

exception Empty

val empty : card list
(**[empty] is the empty card list*)

val is_empty : card list -> bool
(** [is_empty hand] is [true] when card list [hand] is the empty card
    list and is [false] otherwise.*)

val deal_card : card list -> card
(**[deal_card deck] is the top card of the card list/deck [deck]*)

val deal_left : card list -> card list
(**[deal_left deck] is the remaining card list/deck when when the top
   card of [deck] is removed. *)

val add_card : card -> card list -> card list
(** [add_card card hand] is the list of cards containing [card] and all
    the cards in [hand] *)

val size : card list -> int
(** [size crd_lst] is the amount of cards in the card list [crd_lst]*)

val to_list : card list -> card list
(** [to_list crd_lst] is the list representation of crd_lst*)

val create_deck : card list
(**[create_deck] creates a full 52-card deck, including all numbers
   between 1-10, all face cards, of all suits*)

val shuffle : card list -> card list
(**[shuffle deck] is the shuffled version of the card list [deck],
   meaning every card in the card list is in a random position/order.*)

val deck_no_hand : card list -> card list -> card list
