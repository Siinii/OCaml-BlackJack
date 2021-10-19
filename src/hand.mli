open Types

exception Empty

val empty : card list

val is_empty : card list -> bool

(* val value_of_card : card -> int *)

val hit : card -> card list -> card list

(* val hand_value : card list -> int *)

val hand_value : card list -> int

val to_list : card list -> card list

val val_to_string : Types.value -> string
