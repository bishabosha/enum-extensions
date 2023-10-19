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
      assertEquals(EnumMirror[E].valueOfUnsafe(name), expected)

    def testFromOrdinal[E: EnumMirror](ordinal: Int, expected: E)(using munit.Location) =
      assertEquals(EnumMirror[E].fromOrdinalUnsafe(ordinal), expected)

    def testDeclaresName[E: EnumMirror](name: String)(using munit.Location) =
      assertEquals(EnumMirror[E].declaresName(name), true)

    def testDeclaresOrdinal[E: EnumMirror](ordinal: Int)(using munit.Location) =
      assertEquals(EnumMirror[E].declaresOrdinal(ordinal), true)

    assertEquals(EnumMirror[Color].mirroredName, "example.ReflectionSuite$.Color")
    assertEquals(EnumMirror[Color].size, 3)
    assertEquals(EnumMirror[Color].values.toSeq, Seq(Red, Green, Blue))
    testValueOf("Red", Red)
    testValueOf("Green", Green)
    testValueOf("Blue", Blue)
    testDeclaresName[Color]("Red")
    testDeclaresName[Color]("Green")
    testDeclaresName[Color]("Blue")
    testDeclaresOrdinal[Color](0)
    testDeclaresOrdinal[Color](1)
    testDeclaresOrdinal[Color](2)
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
