package enumextensions

import EnumMirror._

import scala.collection.immutable.NumericRange
import scala.util.Try
import scala.quoted._

trait NumericOps[T](using final val mirror: EnumMirror[T]) extends Numeric[T] with Integral[T] { self =>

  final def parseString(str: String): Option[T] = Try(valueOf(str)).toOption

  extension (t: T) {
    def to (u: T): NumericRange[T]    = NumericRange.inclusive(t, u, one)(self)
    def until (u: T): NumericRange[T] = NumericRange(t, u, one)(self)
  }

}

object NumericOps {

  trait Singleton[T] extends NumericOps[T] {

    final def compare(l: T, r: T): Int = 0

    override final def one = zero

    final def fromInt(x: Int): T = zero

    final def minus(x: T, y: T): T = x
    final def plus(x: T, y: T): T = x
    final def times(x: T, y: T): T = x
    final def quot(x: T, y: T): T = x
    final def rem(x: T, y: T): T = x
    final def negate(x: T): T = x

    final def toDouble(x: T): Double = 0
    final def toFloat(x: T): Float = 0
    final def toInt(x: T): Int = 0
    final def toLong(x: T): Long = 0

  }

  trait Modular[T] extends NumericOps[T] {
    import mirror.size

    final def compare(l: T, r: T): Int = l.ordinal - r.ordinal

    final def minus(x: T, y: T): T = fromOrdinal((size + 1 + x.ordinal - y.ordinal) % size)
    final def plus(x: T, y: T): T = fromOrdinal((x.ordinal + y.ordinal) % size)
    final def times(x: T, y: T): T = fromOrdinal((x.ordinal * y.ordinal) % size)
    final def quot(x: T, y: T): T = fromOrdinal((x.ordinal / y.ordinal) % size)
    final def rem(x: T, y: T): T = fromOrdinal((x.ordinal % y.ordinal) % size)
    final def negate(x: T): T = fromOrdinal((size - x.ordinal) % size)

    final def fromInt(x: Int): T = fromOrdinal((x + size) % size)

    final def toDouble(x: T): Double = x.ordinal.toDouble
    final def toFloat(x: T): Float = x.ordinal.toFloat
    final def toInt(x: T): Int = x.ordinal
    final def toLong(x: T): Long = x.ordinal.toLong

  }

  transparent inline def derived[T](using inline mirror: EnumMirror[T]): NumericOps[T] =
    ${ derivedNumericOps[T]('mirror) }

  def derivedNumericOps[T: Type](mirror: Expr[EnumMirror[T]])(using QuoteContext): Expr[NumericOps[T]] =
    import qctx.tasty._

    val tpe = typeOf[T]

    val sym = tpe.classSymbol match
      case Some(sym) => sym
      case _         => report.throwError(s"${tpe.show} is not a class type")

    if sym.children.length > 1 then '{
      new NumericOps(using $mirror) with NumericOps.Modular[T] {
        override final val zero = fromOrdinal(0)
      }
    }
    else '{
      new NumericOps(using $mirror) with NumericOps.Singleton[T] {
        override final val zero = fromOrdinal(0)
        override final val one  = fromOrdinal(1)
      }
    }
}
