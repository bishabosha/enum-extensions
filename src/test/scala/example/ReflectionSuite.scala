package example

import enumextensions.EnumMirror

import ReflectionSuite.*

object ReflectionSuite {
  enum Color derives EnumMirror {
    case Red, Green, Blue
  }
}

class ReflectionSuite extends munit.FunSuite {

  test("EnumMirror Color") {
    import Color.{Red, Green, Blue}

    def testName[E: EnumMirror](e: E, expected: String)(using munit.Location) =
      assertEquals(e.name, expected)

    def testOrdinal[E: EnumMirror](e: E, expected: Int)(using munit.Location) =
      assertEquals(e.ordinal, expected)

    def testValueOf[E: EnumMirror](name: String, expected: E)(using munit.Location) =
      assertEquals(EnumMirror[E].valueOf(name), expected)

    def testFromOrdinal[E: EnumMirror](ordinal: Int, expected: E)(using munit.Location) =
      assertEquals(EnumMirror[E].fromOrdinal(ordinal), expected)

    assertEquals(EnumMirror[Color].mirroredName, "example.ReflectionSuite$.Color")
    assertEquals(EnumMirror[Color].size, 3)
    assertEquals(EnumMirror[Color].values.toSeq, Seq(Red, Green, Blue))
    testValueOf("Red", Red)
    testValueOf("Green", Green)
    testValueOf("Blue", Blue)
    testFromOrdinal(0, Red)
    testFromOrdinal(1, Green)
    testFromOrdinal(2, Blue)
    testName(Red, "Red")
    testName(Green, "Green")
    testName(Blue, "Blue")
    testOrdinal(Red, 0)
    testOrdinal(Green, 1)
    testOrdinal(Blue, 2)
  }

}
