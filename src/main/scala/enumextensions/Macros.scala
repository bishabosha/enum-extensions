package enumextensions

import scala.quoted.*
import scala.collection.SeqView
import scala.deriving.Mirror

object Macros:

  def string[T: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    val ConstantType(StringConstant(str)) = TypeRepr.of[T]: @unchecked
    Expr(str)


  def names[T: Type](using Quotes): List[Expr[String]] = Type.of[T] match
    case '[EmptyTuple] => Nil
    case '[t *: ts] => string[t] :: names[ts]


  def derivedEnumMirror[E: Type](using Quotes): Expr[EnumMirror[E]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[E]

    val sym = tpe.classSymbol match
      case Some(sym) if sym.flags.is(Flags.Enum) && !sym.flags.is(Flags.JavaDefined) =>
        sym
      case _ =>
        report.errorAndAbort(s"${tpe.show} is not an enum type")

    val M = Expr.summon[Mirror.SumOf[E]] match
      case Some(mirror) => mirror
      case None         => report.errorAndAbort(s"Could not summon a Mirror.SumOf[${tpe.show}]")

    val reifiedNames: Expr[Set[String]] = M match
      case '{ $m: Mirror.SumOf[E] { type MirroredElemLabels = elemLabels  } } =>
        '{ Set(${ Varargs(names[elemLabels]) }*) }

    val E = sym.companionModule

    val valuesRef =
      Select.unique(Ref(E), "values").asExprOf[Array[E & reflect.Enum]]

    def reifyValueOf(name: Expr[String]) =
      Select.overloaded(Ref(E), "valueOf", Nil, name.asTerm::Nil).asExprOf[E & reflect.Enum]

    def reifyFromOrdinal(ordinal: Expr[Int]) =
      Select.overloaded(Ref(E), "fromOrdinal", Nil, ordinal.asTerm::Nil).asExprOf[E & reflect.Enum]

    val sizeExpr = Expr(sym.children.length)

    val mirroredNameExpr = Expr(sym.fullName)

    '{

      new EnumMirror[E]:

        private val _values: IArray[E & reflect.Enum] = IArray.unsafeFromArray($valuesRef)
        private val _ordinals = _values.indices
        private val _names = $reifiedNames

        locally:
          assert(_values.length == $sizeExpr)
          assert((_values: IndexedSeq[E & reflect.Enum]).map(_.ordinal).corresponds(_ordinals)(_ == _))

        final def mirroredName: String = $mirroredNameExpr
        final def size: Int = $sizeExpr
        final def values: IArray[E] = _values
        final def declaresOrdinal(ordinal: Int): Boolean = _ordinals.contains(ordinal)
        final def declaresName(name: String): Boolean = _names.contains(name)
        final def valueOfUnsafe(name: String): E = ${ reifyValueOf('name) }
        final def fromOrdinalUnsafe(ordinal: Int): E = ${ reifyFromOrdinal('ordinal) }

        extension (e: E & scala.reflect.Enum)
          final def ordinal: Int = e.ordinal
          final def name: String = e.productPrefix

      end new

    }

  end derivedEnumMirror

end Macros
