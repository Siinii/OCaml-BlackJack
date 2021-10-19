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