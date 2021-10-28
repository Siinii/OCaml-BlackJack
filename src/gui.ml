open Deck
open Hand
open Types
open Player
open Graphics

(* GUI Section *)
let card_pic =
  [ (160, 150); (240, 150); (240, 210); (160, 210) ] |> Array.of_list

let open_gui = open_graph ""

let draw_card (card : card) =
  set_color Graphics.magenta;
  fill_poly card_pic

let rec draw_hand (hand : card list) =
  match hand with
  | [] -> ()
  | h :: t ->
      draw_card h;
      draw_hand t
