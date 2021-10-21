open Types

exception Empty

let empty = []

let is_empty lst =
  match lst with
  | [] -> true
  | _ -> false

let hit (c : card) lst = List.cons c lst

let value_of_card (c : card) =
  match c with
  | { valu = Ace; _ } -> 11
  | { valu = Number x; _ } -> x
  | { valu = _; _ } -> 10

let rec hand_total hand =
  match hand with
  | [] -> 0
  | h :: t -> value_of_card h + hand_total t

let to_list = Fun.id

(* let val_to_string value = match value with | Ace -> "Ace" | King ->
   "King" | Queen -> "Queen" | Jack -> "Jack" | Number x ->
   string_of_int x

   let suit_to_string suit = match suit with | Hearts -> "Hearts" |
   Diamonds -> "Diamonds" | Spades -> "Spades" | Clubs -> "Clubs"

   let card_to_string card = match card with | { valu; suit } ->
   val_to_string valu ^ "of" ^ suit_to_string suit *)

(* let rec hand_string (hand : card list) = match hand with | [] -> "" |
   h :: t -> card_to_string h ^ " , " ^ hand_string t *)
