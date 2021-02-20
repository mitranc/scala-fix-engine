package org.fix

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FixMessageTest extends AnyFunSuite with Matchers {
  private val m = FixMessage(FixSchema(), List(
    Field(35, "R"),
    Group(146, List(
      List(
        Field(711, "1"),
        Field(537, "0")
      ),
      List(
        Field(711, "1"),
        Field(537, "0")
      )
    ))))
  test("explain message as indented text") {
    m.explain().mkString("\n") shouldBe
      """|35 -> R
         |146 -> 2
         |    711 -> 1
         |    537 -> 0
         |    711 -> 1
         |    537 -> 0""".stripMargin
  }
}
