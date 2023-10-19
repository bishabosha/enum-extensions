package enumextensions.numeric

import enumextensions.EnumMirror

import scala.collection.immutable.NumericRange
import scala.util.Try
import scala.quoted.*

trait NumericOps[T](using final val mirror: EnumMirror[T]) extends Numeric[T] with Integral[T] { self =>

  final def parseString(str: String): Option[T] = EnumMirror[T].valueOf(str)

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

    final def compare(l: T, r: T): Int = l.ordinal compare r.ordinal

    final def minus(x: T, y: T): T = EnumMirror[T].fromOrdinalUnsafe((size + 1 + x.ordinal - y.ordinal) % size)
    final def plus(x: T, y: T): T = EnumMirror[T].fromOrdinalUnsafe((x.ordinal + y.ordinal) % size)
    final def times(x: T, y: T): T = EnumMirror[T].fromOrdinalUnsafe((x.ordinal * y.ordinal) % size)
    final def quot(x: T, y: T): T = EnumMirror[T].fromOrdinalUnsafe((x.ordinal / y.ordinal) % size)
    final def rem(x: T, y: T): T = EnumMirror[T].fromOrdinalUnsafe((x.ordinal % y.ordinal) % size)
    final def negate(x: T): T = EnumMirror[T].fromOrdinalUnsafe((size - x.ordinal) % size)

    final def fromInt(x: Int): T = EnumMirror[T].fromOrdinalUnsafe((x + size) % size)

    final def toDouble(x: T): Double = x.ordinal.toDouble
    final def toFloat(x: T): Float = x.ordinal.toFloat
    final def toInt(x: T): Int = x.ordinal
    final def toLong(x: T): Long = x.ordinal.toLong

  }

  transparent inline def derived[T](using inline mirror: EnumMirror[T]): NumericOps[T] =
    ${ Macros.derivedNumericOps[T]('mirror) }
}
