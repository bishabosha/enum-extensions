import enumextensions._
import EnumMirror._

@main def Main: Unit = {
  println(Color.Red)
  doStuff(Color.Green)
  println(Test.msg)
}

def doStuff[E <: AnyRef: EnumMirror](e: E): Unit =
  assert(valueOf(e.name) `eq` e)
  println(s"""{"${e.name}": ${e.ordinal}}""")


object Test {
  def msg = "I was compiled by dotty :)"
}
