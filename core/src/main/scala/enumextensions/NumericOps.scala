package enumextensions
import quoted._

trait NumericOps[T] extends Numeric[T] with Integral[T] {

  extension (t: T) def repr: Int

  final def compare(l: T, r: T): Int = l.repr - r.repr

  def size: Int

  final def minus(x: T, y: T): T = fromInt((size + 1 + x.repr - y.repr) % size)
  final def plus(x: T, y: T): T = fromInt((x.repr + y.repr) % size)
  final def times(x: T, y: T): T = fromInt((x.repr * y.repr) % size)
  final def quot(x: T, y: T): T = fromInt((x.repr / y.repr) % size)
  final def rem(x: T, y: T): T = fromInt((x.repr % y.repr) % size)
  final def negate(x: T): T = fromInt((size - x.repr) % size)
  final def toDouble(x: T): Double = x.repr.toDouble
  final def toFloat(x: T): Float = x.repr.toFloat
  final def toInt(x: T): Int = x.repr
  final def toLong(x: T): Long = x.repr.toLong

}

object NumericOps {
  import scala.collection.immutable.NumericRange

  given RangeOps as AnyRef {
    extension [T: NumericOps](t: T) {
      def to (u: T): NumericRange[T] = NumericRange.inclusive(t, u, summon[NumericOps[T]].one)
      def until (u: T): NumericRange[T] = NumericRange(t, u, summon[NumericOps[T]].one)
    }
  }

  transparent inline def derived[T](using inline enumerated: Enumerated[T]): NumericOps[T] =
    ${ derivedNumericOps[T]('enumerated) }

  def derivedNumericOps[T: Type](enumerated: Expr[Enumerated[T]])(using QuoteContext): Expr[NumericOps[T]] =
    import qctx.tasty._

    val tpe = typeOf[T]

    val sym = tpe.classSymbol match
      case Some(sym) => sym
      case _         => report.throwError(s"${tpe.show} is not a class type")

    if sym.children.length < 2 then // Numeric APIs expect `T` to *not* be singleton
      report.throwError(s"enum ${tpe.show} needs at least 2 cases to derive ${quoted.Type[NumericOps[T]].show}")

    '{
      new {
        import Enumerated._

        given Enumerated[T] = $enumerated

        extension (t: T) def repr: Int = t.ordinal

        final val size = summon[Enumerated[T]].size

        def fromInt(x: Int): T = values(x)
        def parseString(str: String): Option[T] = scala.util.Try(valueOf(str)).toOption
      }
    }
}
