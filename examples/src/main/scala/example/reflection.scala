package example

import enumextensions._
import EnumMirror._

@main def reflectEnums: Unit =
  pprint.pprintln(reflectOn[Color], width = 20)

def reflectOn[E <: AnyRef: EnumMirror] =
  Map(
    "enum" -> enumMirror.mirroredName,
    "size" -> enumMirror.size,
    "values" -> enumMirror.values.map(e => e.name -> e.ordinal)
  )


def msg = "I was compiled by dotty :)"
