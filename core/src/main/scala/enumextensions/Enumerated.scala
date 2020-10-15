package enumextensions

import quoted._

trait Enumerated[E] {

  // reify the constructor state as a tuple
  // type Data <: Tuple

  def values: IArray[E]
  def valueOf(name: String): E
  // def fromOrdinal(ordinal: Int): E

  extension (e: E)
    def ordinal: Int
    def name: String
    // def data: Data

}

object Enumerated {

  inline def values[E](using E: Enumerated[E]): IArray[E] = E.values
  inline def valueOf[E](name: String)(using E: Enumerated[E]): E = E.valueOf(name)

  transparent inline def derived[E]: Enumerated[E] = ${ derivedImpl }

  def derivedImpl[E: Type](using QuoteContext): Expr[Enumerated[E]] =
    import qctx.tasty._

    val tpe = typeOf[E]

    val sym = tpe.classSymbol match
      case Some(sym) => sym
      case _         => report.throwError(s"${tpe.show} is not an enum type")

    if !sym.flags.is(Flags.Enum)
      report.throwError(s"${tpe.show} is not an enum type")

    val E = sym.companionModule

    val E_values                      = Select.unique(Ref(E), "values").seal.cast[Array[E]]
    def E_valueOf(name: Expr[String]) = Select.overloaded(Ref(E), "valueOf", Nil, name.unseal::Nil).seal.cast[E]

    '{

      new Enumerated[E] {

        def values: IArray[E] = $E_values.asInstanceOf[IArray[E]]
        def valueOf(name: String): E = ${E_valueOf('name)}
        // def fromOrdinal(ordinal: Int): E

        extension (e: E & scala.Enum)
          def ordinal: Int = e.ordinal
          def name: String = e.productPrefix

      }
    }
}
