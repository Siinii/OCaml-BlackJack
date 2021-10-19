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

let peek = function
  | [] -> raise Empty
  | x :: _ -> x

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
let rec shuffle (deck : card list) =
  let rand_deck = List.map (fun c -> (Random.bits (), c)) deck in
  let snd_deck = List.sort compare rand_deck in
  List.map snd snd_deck