package example

import NumericSuite.*
import Rank.*
import Suit.*

import enumextensions.EnumMirror
import enumextensions.numeric.NumericOps

import scala.collection.immutable.NumericRange

object NumericSuite:

  enum Single derives EnumMirror, NumericOps:
    case One

  enum Rank derives EnumMirror, NumericOps:
    case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen,
      King, Ace

  enum Suit derives EnumMirror, NumericOps:
    case Clubs, Diamonds, Hearts, Spades

  case class Card(suit: Suit, rank: Rank)
end NumericSuite

class NumericSuite extends munit.FunSuite:

  test("make deck of cards"):
    val deck =
      for
        suit <- Clubs to Spades
        rank <- Two to Ace
      yield Card(suit, rank)

    val deck2 =
      for
        suit <- EnumMirror[Suit].values
        rank <- EnumMirror[Rank].values
      yield Card(suit, rank)

    assertEquals(
      deck,
      deck2.toIndexedSeq
    )
  .endLocally

  test("test is numeric"):
    def rangeTo[E: Integral](from: E, to: E): NumericRange[E] =
      NumericRange.inclusive(from, to, summon[Numeric[E]].one)
    assertEquals(
      rangeTo(Clubs, Spades).toIndexedSeq,
      EnumMirror[Suit].values.toIndexedSeq
    )
  .endLocally

end NumericSuite
