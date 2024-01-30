# Enum Extensions
![CI status](https://github.com/bishabosha/enum-extensions/actions/workflows/ci.yml/badge.svg)
[![javadoc](https://javadoc.io/badge2/io.github.bishabosha/enum-extensions_3/latest_documentation.svg)](https://javadoc.io/doc/io.github.bishabosha/enum-extensions_3)

The Enum Extensions library provides type classes to work generically with enumerations in Scala 3

## Usage

Type classes are provided in the `enumextensions` package.

### Enum Mirror

`EnumMirror[E]` is a type class that provides reflection over an enumeration type `E`. It provides several capabilities:
- cached `values` as an `IArray[E]`
- reflection of `name` (`String`) or `ordinal` (`Int`) for any individual case.
- safe lookup individual cases by `name` or `ordinal`,
- unsafe (efficient) lookup of individual cases by `name` or `ordinal`.

See the above use cases in action below:

```scala
import enumextensions.EnumMirror

def enumName[E: EnumMirror]: String =
  EnumMirror[E].mirroredName // e.g. example.Color

def sortedCases[E: EnumMirror]: IArray[E] =
  EnumMirror[E].values // cached IArray

def nameOrdinalPairs[E: EnumMirror]: Map[String, Int] =
  Map.from( //                      ┌ NAME    ┌ ORDINAL
    for e <- sortedCases[E] yield e.name -> e.ordinal
  )

// Lookups returning `Option[E]`
def safeLookup[E: EnumMirror](name: String): Option[E] =
  EnumMirror[E].valueOf(name)
def safeLookup[E: EnumMirror](ordinal: Int): Option[E] =
  EnumMirror[E].fromOrdinal(ordinal)

// assert that name/ordinal exists for convenience
def unsafeLookup[E: EnumMirror](name: String): E =
  EnumMirror[E].valueOfUnsafe(name)
def unsafeLookup[E: EnumMirror](ordinal: Int): E =
  EnumMirror[E].fromOrdinalUnsafe(ordinal)
```

`given` instances of `EnumMirror` are not provided automatically, you must explicitly opt in as follows:

```scala
enum Color derives EnumMirror:
  case Red, Green, Blue
```

### Numeric

In the `enumextensions.numeric` package we provide `NumericOps`, which extends types with a given `EnumMirror[E]` into a given `scala.math.Integral[E]`, as well as providing standard numeric operations over the enum, it is simple to create sub-ranges of values.

Let's define a `WeekDays` enumeration, opting into numeric derivation by `derives NumericOps`, and declare ranges for both the `daysOfWeek` and `weekend`:

```scala
import enumextensions.EnumMirror
import enumextensions.numeric.NumericOps

enum WeekDays derives EnumMirror, NumericOps:
  case Monday, Tuesday, Wednesday, Thursday, Friday
  case Saturday, Sunday

object WeekDays:
  val daysOfWeek = Monday to Friday
  val weekend    = Saturday to Sunday
```

here is a demonstration of using the numeric operators, e.g.
```scala
scala> -(-Wednesday) == Wednesday
true
```

