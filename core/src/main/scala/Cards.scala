import enumextensions._

import Rank._
import Suit._

enum Rank derives EnumMirror, NumericOps {
  case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace
}

enum Suit derives EnumMirror, NumericOps {
  case Clubs, Diamonds, Hearts, Spades
}

case class Card(suit: Suit, rank: Rank)

def Deck =
  for
    suit <- Clubs to Spades
    rank <- Two   to Ace
  yield Card(suit, rank)
