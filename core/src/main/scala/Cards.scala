import enumextensions._
import NumericOps.{given _}

import Rank._
import Suit._

enum Rank derives Enumerated, NumericOps {
  case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace
}

enum Suit derives Enumerated, NumericOps {
  case Clubs, Diamonds, Hearts, Spades
}

case class Card(suit: Suit, rank: Rank)

def Deck =
  for
    suit <- Clubs to Spades
    rank <- Two   to Ace
  yield Card(suit, rank)
