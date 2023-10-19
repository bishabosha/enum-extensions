package enumextensions

trait EnumMirror[E]:

  def mirroredName: String
  def size: Int
  def values: IArray[E]
  def declaresOrdinal(ordinal: Int): Boolean
  def declaresName(name: String): Boolean
  def valueOfUnsafe(name: String): E
  def fromOrdinalUnsafe(ordinal: Int): E
  def valueOf(name: String): Option[E] =
    if declaresName(name) then Some(valueOfUnsafe(name)) else None
  def fromOrdinal(ordinal: Int): Option[E] =
    if declaresOrdinal(ordinal) then Some(fromOrdinalUnsafe(ordinal)) else None

  extension (e: E)
    def ordinal: Int
    def name: String

object EnumMirror:

  inline def apply[E](using mirror: EnumMirror[E]): mirror.type = mirror

  transparent inline def derived[E]: EnumMirror[E] = ${ Macros.derivedEnumMirror[E] }

end EnumMirror
