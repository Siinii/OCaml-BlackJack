type suit =
  | Hearts
  | Spades
  | Clubs
  | Diamonds

type value =
  | Ace
  | King
  | Queen
  | Jack
  | Number of int

type card = {
  valu : value;
  suit : suit;
}

type round =
  | Start
  | P1_bet
  | P2_bet
  | Deal
  | P1_hit
  | P2_hit
  | Dealer_hit
  | Winner
  | End

type game_state = {
  round : round;
  curr_deck : card list;
  p1_hand : card list;
  p2_hand : card list;
  dealer_hand : card list;
  p1_earnings : int;
  p2_earnings : int;
  p1_bet : int;
  p2_bet : int;
}
(** TODO: How to make this work with more than 2 players?*)
