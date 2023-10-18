package enumextensions

import scala.quoted.*

object Macros:

  def derivedEnumMirror[E: Type](using Quotes): Expr[EnumMirror[E]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[E]

    val sym = tpe.classSymbol match
      case Some(sym) if sym.flags.is(Flags.Enum) && !sym.flags.is(Flags.JavaDefined) =>
        sym
      case _ =>
        report.errorAndAbort(s"${tpe.show} is not an enum type")

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

        private val _values: IArray[E] = IArray.unsafeFromArray($valuesRef)

        final def mirroredName: String = $mirroredNameExpr
        final def size: Int = $sizeExpr
        final def values: IArray[E] = _values
        final def valueOf(name: String): E = ${ reifyName('name) }
        final def fromOrdinal(ordinal: Int): E = ${ reifyOrdinal('ordinal) }

        extension (e: E & scala.reflect.Enum)
          final def ordinal: Int = e.ordinal
          final def name: String = e.productPrefix

      end new

    }

  end derivedEnumMirror

end Macros
