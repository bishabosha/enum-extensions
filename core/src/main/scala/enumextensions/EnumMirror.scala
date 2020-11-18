package enumextensions

import scala.quoted._

trait EnumMirror[E] {

  def size: Int
  def values: IArray[E]
  def valueOf(name: String): E
  def fromOrdinal(ordinal: Int): E

  extension (e: E)
    def ordinal: Int
    def name: String

}

object EnumMirror {

  inline def values[E](using E: EnumMirror[E]): IArray[E] = E.values
  inline def valueOf[E](name: String)(using E: EnumMirror[E]): E = E.valueOf(name)
  inline def fromOrdinal[E](ordinal: Int)(using E: EnumMirror[E]): E = E.fromOrdinal(ordinal)

  transparent inline def derived[E]: EnumMirror[E] = ${ derivedEnumMirror[E] }

  def derivedEnumMirror[E: Type](using QuoteContext): Expr[EnumMirror[E]] =
    import qctx.reflect._

    val tpe = TypeRepr.of[E]

    val sym = tpe.classSymbol match
      case Some(sym) => sym
      case _         => report.throwError(s"${tpe.show} is not an enum type")

    if !sym.flags.is(Flags.Enum) then
      report.throwError(s"${tpe.show} is not an enum type")

    val E = sym.companionModule

    val valuesRef =
      Select.unique(Ref(E), "values").seal.cast[Array[E]]

    def reifyName(name: Expr[String]) =
      Select.overloaded(Ref(E), "valueOf", Nil, name.unseal::Nil).seal.cast[E]

    def reifyOrdinal(ordinal: Expr[Int]) =
      Select.overloaded(Ref(E), "fromOrdinal", Nil, ordinal.unseal::Nil).seal.cast[E]

    val sizeExpr = Expr(sym.children.length)

    '{

      new EnumMirror[E] {

        final def size: Int = $sizeExpr
        final def values: IArray[E] = IArray.unsafeFromArray($valuesRef)
        final def valueOf(name: String): E = ${ reifyName('name) }
        final def fromOrdinal(ordinal: Int): E = ${ reifyOrdinal('ordinal) }

        extension (e: E & scala.Enum) {
          final def ordinal: Int = e.ordinal
          final def name: String = e.productPrefix
        }

      }

    }
}
