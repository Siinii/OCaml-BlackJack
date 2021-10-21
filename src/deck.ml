open Types

exception Empty

let empty = []

let is_empty lst =
  match lst with
  | [] -> true
  | _ -> false

let deal_card lst =
  match lst with
  | [] -> raise Empty
  | h :: _ -> h

let deal_left lst =
  match lst with
  | [] -> raise Empty
  | _ :: s -> s

let add_card = List.cons

let size = List.length

let to_list = Fun.id

let rank_to_value rank =
  match rank with
  | 14 -> Ace
  | 13 -> King
  | 12 -> Queen
  | 11 -> Jack
  | x when x > 1 && x <= 10 -> Number x
  | _ -> failwith "CardDoesNotExist"

let create_suit_vals suit =
  List.map
    (fun x -> { valu = rank_to_value x; suit })
    [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ]

(** [create_deck] is a full deck of 52 cards, with 13 cards of each
    suit, from 2-Ace*)
let create_deck =
  List.flatten
    (List.map create_suit_vals [ Hearts; Spades; Clubs; Diamonds ])

(* Must be redone *)
let rand_int inte = Random.int inte

let rec shuffle (deck : card list) =
  Random.self_init ();
  let rand_deck = List.map (fun c -> (Random.bits (), c)) deck in
  let snd_deck = List.sort compare rand_deck in
  List.map snd snd_deck

let card_in_deck (hand : card) (lst : card list) =
  if List.mem hand lst then true else false

let remove_card (card : card) (deck : card list) =
  match deck with
  | [] -> []
  | h :: t -> if h = card then t else h :: t

let rec deck_no_hand hand deck =
  match hand with
  | [] -> deck
  | h :: t ->
      if card_in_deck h deck then deck_no_hand t (remove_card h deck)
      else deck
