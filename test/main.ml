open OUnit2
open Game.Types
open Game.Deck
open Game.Hand

let og_deck = Game.Deck.create_deck

let shuffled = Game.Deck.shuffle og_deck

let card_one = Game.Deck.deal_card shuffled

let rem_deck = Game.Deck.deal_left shuffled

let card_two = Game.Deck.deal_card rem_deck

let list_of_cards_test = [ card_one; card_two ]

let rem_deck_second = Game.Deck.deal_left rem_deck

let card_three = Game.Deck.deal_card rem_deck_second

let rem_deck_third = Game.Deck.deal_left rem_deck_second

let card_four : card = Game.Deck.deal_card rem_deck_third

let deck_example = [ card_one; card_two; card_three; card_four ]

let deck_of_cards = create_deck

let isTrue = shuffle deck_of_cards != deck_of_cards

let deck_minus_hand =
  deck_no_hand [ card_one; card_two ] [ card_one; card_two; card_three ]

let deck_tests =
  [
    ( "is_empty true" >:: fun _ ->
      assert_equal (Game.Deck.is_empty []) true );
    ( "is_empty false" >:: fun _ ->
      assert_equal (Game.Deck.is_empty list_of_cards_test) false );
    ( "deal_card" >:: fun _ ->
      assert_equal (deal_card list_of_cards_test) card_one );
    ( "deal_card_empty_list" >:: fun _ ->
      assert_raises Game.Deck.Empty (fun () -> Game.Deck.deal_card [])
    );
    ( "add_card_test" >:: fun _ ->
      assert_equal (add_card card_one [ card_two ]) list_of_cards_test
    );
    ( "peek_test" >:: fun _ ->
      assert_equal (peek list_of_cards_test) card_one );
    ( "peek_test_empty_list" >:: fun _ ->
      assert_raises Game.Deck.Empty (fun () -> Game.Deck.peek []) );
    ("size_test" >:: fun _ -> assert_equal (size list_of_cards_test) 2);
    ( "to_list_test" >:: fun _ ->
      assert_equal [ card_one; card_two ] list_of_cards_test );
    ("shuffle_test" >:: fun _ -> assert_equal isTrue true);
    ( "deck_minus_test" >:: fun _ ->
      assert_equal deck_minus_hand [ card_three ] );
  ]

let hand_tests =
  [
    ( "is_empty true" >:: fun _ ->
      assert_equal (Game.Hand.is_empty []) true );
    ( "is_empty false" >:: fun _ ->
      assert_equal (Game.Hand.is_empty list_of_cards_test) false );
    ( "hit_test" >:: fun _ ->
      assert_equal
        (hit card_three list_of_cards_test)
        [ card_three; card_one; card_two ] );
    ( "hand_value_test" >:: fun _ ->
      assert_equal (hand_value deck_of_cards) 380 );
  ]

let suite =
  "test suite for blackjackfp"
  >::: List.flatten [ deck_tests; hand_tests ]

let _ = run_test_tt_main suite