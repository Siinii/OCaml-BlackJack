open Types

exception Empty

val empty : card list

val is_empty : card list -> bool

val deal_card : card list -> card

val deal_left : card list -> card list

val add_card : card -> card list -> card list

val peek : card list -> card

val size : card list -> int

val to_list : card list -> card list

val create_deck : card list

val shuffle : card list -> card list

val deck_no_hand : card list -> card list -> card list
