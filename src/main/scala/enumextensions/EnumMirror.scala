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

  inline def apply[E](using mirror: EnumMirror[E]): mirror.type = mirror

  transparent inline def derived[E]: EnumMirror[E] = ${ Macros.derivedEnumMirror[E] }

end EnumMirror
