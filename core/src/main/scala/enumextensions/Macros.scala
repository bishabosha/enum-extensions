package enumextensions

import scala.quoted._

object Macros:

  def derivedEnumMirror[E: Type](using Quotes): Expr[EnumMirror[E]] =
    import quotes.reflect._

    val tpe = TypeRepr.of[E]

    val sym = tpe.classSymbol match
      case Some(sym) if sym.flags.is(Flags.Enum) && !sym.flags.is(Flags.JavaDefined) =>
        sym
      case _ =>
        report.throwError(s"${tpe.show} is not an enum type")

    val E = sym.companionModule

    val valuesRef =
      Select.unique(Ref(E), "values").asExprOf[Array[E]]

    def reifyName(name: Expr[String]) =
      Select.overloaded(Ref(E), "valueOf", Nil, name.asTerm::Nil).asExprOf[E]

    def reifyOrdinal(ordinal: Expr[Int]) =
      Select.overloaded(Ref(E), "fromOrdinal", Nil, ordinal.asTerm::Nil).asExprOf[E]

    val sizeExpr = Expr(sym.children.length)

    val mirroredNameExpr = Expr(sym.fullName)

    '{

      new EnumMirror[E]:

        final def mirroredName: String = $mirroredNameExpr
        final def size: Int = $sizeExpr
        final def values: IArray[E] = IArray.unsafeFromArray($valuesRef)
        final def valueOf(name: String): E = ${ reifyName('name) }
        final def fromOrdinal(ordinal: Int): E = ${ reifyOrdinal('ordinal) }

        extension (e: E & scala.reflect.Enum)
          final def ordinal: Int = e.ordinal
          final def name: String = e.productPrefix

      end new

    }

  end derivedEnumMirror

  def derivedNumericOps[T: Type](mirror: Expr[EnumMirror[T]])(using Quotes): Expr[NumericOps[T]] =
    import quotes.reflect._
    import EnumMirror._

    val tpe = TypeRepr.of[T]

    val sym = tpe.classSymbol match
      case Some(sym) => sym
      case _         => report.throwError(s"${tpe.show} is not a class type")

    if sym.children.length > 1 then ('{
      new NumericOps(using $mirror) with NumericOps.Modular[T]:
        override final val zero = fromOrdinal(0)
    })
    else ('{
      new NumericOps(using $mirror) with NumericOps.Singleton[T]:
        override final val zero = fromOrdinal(0)
        override final val one  = fromOrdinal(1)
    })
  end derivedNumericOps

end Macros
