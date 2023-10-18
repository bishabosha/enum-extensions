package enumextensions.numeric

import enumextensions.EnumMirror

import scala.quoted.*

object Macros:

  def derivedNumericOps[T: Type](mirror: Expr[EnumMirror[T]])(using Quotes): Expr[NumericOps[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    val sym = tpe.classSymbol match
      case Some(sym) => sym
      case _         => report.errorAndAbort(s"${tpe.show} is not a class type")

    if sym.children.length > 1 then ('{
      new NumericOps(using $mirror) with NumericOps.Modular[T]:
        override final val zero = EnumMirror[T].fromOrdinal(0)
        override final val one  = EnumMirror[T].fromOrdinal(1)
    })
    else ('{
      new NumericOps(using $mirror) with NumericOps.Singleton[T]:
        override final val zero = EnumMirror[T].fromOrdinal(0)
    })
  end derivedNumericOps

end Macros
