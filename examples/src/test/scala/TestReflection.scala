package example

import org.junit.Test
import org.junit.Assert._

class Test1 {
  @Test def t1(): Unit = {
    assertEquals("""{"Red": 0, "parent": "example.Color"}""", reflectOn(Color.Red))
  }
}
