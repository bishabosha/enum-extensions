package enumextensions

trait EnumMirror[E]:

  def mirroredName: String
  def size: Int
  def values: IArray[E]
  def valueOf(name: String): E
  def fromOrdinal(ordinal: Int): E

  extension (e: E)
    def ordinal: Int
    def name: String

object EnumMirror:

  inline def values[E](using E: EnumMirror[E]): IArray[E] = E.values
  inline def valueOf[E](name: String)(using E: EnumMirror[E]): E = E.valueOf(name)
  inline def fromOrdinal[E](ordinal: Int)(using E: EnumMirror[E]): E = E.fromOrdinal(ordinal)
  inline def enumMirror[E](using E: EnumMirror[E]): E.type = E

  transparent inline def derived[E]: EnumMirror[E] = ${ Macros.derivedEnumMirror[E] }

end EnumMirror
