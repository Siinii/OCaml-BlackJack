open Types

exception Empty

(**type card = Types.card

   type value = Types.value

   type suit = Types.suit *)

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

let rec hand_value hand =
  match hand with
  | [] -> 0
  | h :: t -> value_of_card h + hand_value t

let to_list = Fun.id

let val_to_string value =
  match value with
  | Ace -> "Ace"
  | King -> "King"
  | Queen -> "Queen"
  | Jack -> "Jack"
  | Number x -> string_of_int x