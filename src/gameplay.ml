open Deck
open Hand
open Types
open Player

type input_phrase = string list

type move =
  | Hit
  | Stand
  | Bet of int
  | PlaceHolder

exception NotCommand

(* Potentially read spaces in later implementation *)

(* let start_game = failwith "Unimplemented" *)

let og_deck = Deck.create_deck

let parse_bet str =
  try int_of_string str with
  | Failure "int_of_string" -> 0

let shuffled = Deck.shuffle og_deck

let init_state : game_state =
  {
    round = Start;
    curr_deck = shuffled;
    p1_hand = Hand.empty;
    p2_hand = Hand.empty;
    dealer_hand = Hand.empty;
    p1_earnings = 500;
    p2_earnings = 500;
    p1_bet = 0;
    p2_bet = 0;
    winner_p1_or_dealer = Unknown;
    winner_p2_or_dealer = Unknown;
  }

let card_one = Deck.deal_card shuffled

let rem_deck = Deck.deal_left shuffled

let card_two = Deck.deal_card rem_deck

let rem_deck_second = Deck.deal_left rem_deck

let card_three = Deck.deal_card rem_deck_second

let rem_deck_third = Deck.deal_left rem_deck_second

let card_four = Deck.deal_card rem_deck_third

let rec n_times f n x = if n > 0 then n_times f (n - 1) (f x) else x

let rem_deck_nth n = n_times Deck.deal_left n shuffled

let card_n n = Deck.deal_card (rem_deck_nth (n - 1))

let give_init_cards =
  let hand = [ card_n 1; card_n 2 ] in
  hand

let ai_init_hand = [ card_n 3; card_n 4 ]

(**What is this supposed to do??*)
(*let hit_card (hand : card list) : card list = Hand.hit (Deck.deal_card
  hand) *)

(* REMOVE LATER *)
let val_to_string value =
  match value with
  | Ace -> "Ace"
  | King -> "King"
  | Queen -> "Queen"
  | Jack -> "Jack"
  | Number x -> string_of_int x

let suit_to_string suit =
  match suit with
  | Hearts -> " Hearts"
  | Diamonds -> " Diamonds"
  | Spades -> " Spades"
  | Clubs -> " Clubs"

let card_to_string card =
  match card with
  | { valu; suit } -> val_to_string valu ^ " of" ^ suit_to_string suit

let rec hand_string (hand : card list) =
  match hand with
  | [] -> ""
  | [ h ] -> card_to_string h
  | h :: t -> card_to_string h ^ " , " ^ hand_string t

(* REMOVE LATER *)
let parse str =
  match str with
  | "hit" -> Hit
  | "h" -> Hit
  | "s" -> Stand
  | "stand" -> Stand
  | "PlaceHolder" -> PlaceHolder
  | _ -> (
      try Bet (int_of_string str) with
      | Failure "int_of_string" -> raise NotCommand)
(* let rec ai_dealer_final_hand cards deck_list = match cards with | []
   -> failwith "Unimplemented" | h :: t -> let hit_hand = Deck.add_card
   (Deck.deal_card deck_list) cards in if Hand.hand_total cards < 17
   then ai_dealer_final_hand hit_hand else cards *)

(* let rec use_command str (cards : card list) = let command = parse str
   in match command with | Hit -> let new_cards = Deck.add_card
   (deal_card (shuffle (deck_no_hand cards create_deck))) cards in if
   hand_total new_cards > 21 then let () = print_endline (hand_string
   new_cards); print_endline "You lose!" in () else let () =
   print_endline ("Your hand is: " ^ hand_string new_cards);
   print_endline ("The Dealer's first card is: " ^ card_to_string
   (deal_card ai_init_hand)); print_endline "Do you want to hit (h) or
   stand (s)?"; match read_line () with | hit -> use_command hit
   new_cards in

   () | Stand -> let dealer_hand = ai_dealer_final_hand ai_init_hand in
   let dealer_val = hand_total dealer_hand in let () = print_endline
   ("Your total is: " ^ string_of_int (hand_total cards)); print_endline
   ("The Dealer's hand is: " ^ hand_string dealer_hand); print_endline
   ("Dealer's total is: " ^ string_of_int dealer_val); if dealer_val <=
   21 && dealer_val > hand_total cards then print_endline "You lose"
   else print_endline "You win" in () *)
(* let make_move move = match move with | [] -> *)

let bet_from_parse move =
  match move with
  | Bet x -> x
  | _ -> 0

let rec new_state (state : game_state) (command : string) : unit =
  if state.round = Start then (
    print_endline "A new round is starting:";
    print_endline ("Player 1 has $" ^ string_of_int state.p1_earnings);
    print_endline ("Player 2 has $" ^ string_of_int state.p2_earnings);

    print_endline "Player 1: Please enter your bet:";
    print_string "> ";
    match read_line () with
    | cmnd -> new_state { state with round = P1_bet } cmnd)
  else if state.round = P1_bet then (
    print_endline
      ("Player 1 bet $" ^ string_of_int (bet_from_parse (parse command)));
    print_endline "Player 2: Please enter your bet:";
    print_string "> ";
    match read_line () with
    | cmnd ->
        new_state
          {
            state with
            round = P2_bet;
            p1_bet = bet_from_parse (parse command);
          }
          cmnd)
  else if state.round = P2_bet then (
    print_endline
      ("Player 1 bet $" ^ string_of_int (bet_from_parse (parse command)));
    print_endline "Now cards will be dealt: ";
    new_state
      {
        state with
        round = Deal;
        p2_bet = bet_from_parse (parse command);
      }
      "PlaceHolder")
  else if state.round = Deal then
    let first_hand = give_init_cards in
    let second_hand = ai_init_hand in
    let dealers_hand = [ card_n 5; card_n 6 ] in
    let remaining_deck = rem_deck_nth 6 in
    new_state
      {
        state with
        round = P1_hit;
        p1_hand = first_hand;
        p2_hand = second_hand;
        dealer_hand = dealers_hand;
        curr_deck = remaining_deck;
      }
      "PlaceHolder"
  else if state.round = P1_hit then (
    print_endline ("Player 1's hand is: " ^ hand_string state.p1_hand);
    print_endline ("Player 2's hand is: " ^ hand_string state.p2_hand);
    print_endline
      ("Dealer's top card is:  "
      ^ card_to_string (Deck.deal_card state.dealer_hand));
    if hand_total state.p1_hand > 21 then (
      print_endline "Player 1 has busted!";
      new_state { state with round = P2_hit } "PlaceHolder")
    else (
      print_endline "Player 1: Would you like to hit (h) or stand (s)?";
      print_string "> ";
      match read_line () with
      | cmnd when parse cmnd = Hit ->
          let first_hand =
            state.p1_hand @ [ Deck.deal_card state.curr_deck ]
          in
          new_state
            {
              state with
              p1_hand = first_hand;
              curr_deck = Deck.deal_left state.curr_deck;
            }
            "PlaceHolder"
      | cmnd when parse cmnd = Stand ->
          new_state { state with round = P2_hit } "PlaceHolder"
      | cmnd ->
          print_endline "Invalid Command; try again.";
          new_state state "PlaceHolder"))
  else if state.round = P2_hit then (
    print_endline ("Player 1's hand is: " ^ hand_string state.p1_hand);
    print_endline ("Player 2's hand is: " ^ hand_string state.p2_hand);
    print_endline
      ("Dealer's top card is:  "
      ^ card_to_string (Deck.deal_card state.dealer_hand));
    if hand_total state.p2_hand > 21 then (
      print_endline "Player 2 has busted!";
      new_state { state with round = Dealer_hit } "PlaceHolder")
    else (
      print_endline "Player 2: Would you like to hit (h) or stand (s)?";
      print_string "> ";
      match read_line () with
      | cmnd when parse cmnd = Hit ->
          let first_hand =
            state.p2_hand @ [ Deck.deal_card state.curr_deck ]
          in
          new_state
            {
              state with
              p2_hand = first_hand;
              curr_deck = Deck.deal_left state.curr_deck;
            }
            "PlaceHolder"
      | cmnd when parse cmnd = Stand ->
          new_state { state with round = Dealer_hit } "PlaceHolder"
      | cmnd ->
          print_endline "Invalid Command; try again.";
          new_state state "PlaceHolder"))
  else if state.round = Dealer_hit then (
    print_endline "Dealer's turn to hit or stand";
    if hand_total state.dealer_hand <= 17 then (
      print_endline "Dealer hits";
      new_state
        {
          state with
          dealer_hand =
            state.dealer_hand @ [ Deck.deal_card state.curr_deck ];
          curr_deck = Deck.deal_left state.curr_deck;
        }
        "PlaceHolder")
    else (
      print_endline "Dealer Stands";
      new_state { state with round = Winner } "PlaceHolder"))
  else if state.round = Winner then (
    print_endline ("Dealer's hand is: " ^ hand_string state.dealer_hand);
    if state.winner_p1_or_dealer = Unknown then (
      if hand_total state.p1_hand > 21 then (
        if hand_total state.dealer_hand <= 21 then (
          print_endline
            ("Player 1 has busted and loses $"
            ^ string_of_int state.p1_bet);
          new_state
            {
              state with
              winner_p1_or_dealer = Dealer;
              p1_earnings = state.p1_earnings - state.p1_bet;
            }
            "PlaceHolder")
        else
          print_endline
            "Both the dealer and Player 1 have busted. It is a draw. \
             Player 1 breaks even.";
        new_state { state with winner_p1_or_dealer = Tie } "PlaceHolder")
      else if
        hand_total state.p1_hand > hand_total state.dealer_hand
        || hand_total state.dealer_hand > 21
      then (
        print_endline
          ("Player 1 beats the dealer and gains $"
          ^ string_of_int state.p1_bet);
        new_state
          {
            state with
            winner_p1_or_dealer = Player1;
            p1_earnings = state.p1_earnings + state.p1_bet;
          }
          "PlaceHolder"))
    else if state.winner_p2_or_dealer = Unknown then
      if hand_total state.p2_hand > 21 then (
        if hand_total state.dealer_hand <= 21 then (
          print_endline
            ("Player 2 has busted and loses $"
            ^ string_of_int state.p2_bet);
          new_state
            {
              state with
              winner_p2_or_dealer = Dealer;
              p2_earnings = state.p2_earnings - state.p2_bet;
            }
            "PlaceHolder")
        else
          print_endline
            "Both the dealer and Player 2 have busted. It is a draw. \
             Player 2 breaks even.";
        new_state { state with winner_p2_or_dealer = Tie } "PlaceHolder")
      else if
        hand_total state.p2_hand > hand_total state.dealer_hand
        || hand_total state.dealer_hand > 21
      then (
        print_endline
          ("Player 2 beats the dealer and gains $"
          ^ string_of_int state.p2_bet);
        new_state
          {
            state with
            winner_p2_or_dealer = Player2;
            p2_earnings = state.p2_earnings + state.p2_bet;
          }
          "PlaceHolder")
      else new_state { state with round = End } "PlaceHolder")
  else if state.round = End then (
    print_endline
      ("Player 1's earnings are: $ " ^ string_of_int state.p1_earnings);
    print_endline
      ("Player 2's earnings are: $" ^ string_of_int state.p2_earnings);
    print_endline "Would you like to play again? (Y/N)";
    print_string "> ";
    match read_line () with
    | str when str = "Y" || str = "y" || str = "yes" ->
        new_state
          {
            init_state with
            p1_earnings = state.p1_earnings;
            p2_earnings = state.p2_earnings;
          }
          "PlaceHolder"
    | str when str = "N" || str = "n" || str = "no" ->
        print_endline "Goodbye!"
    | _ -> print_endline "GoodBye!")
