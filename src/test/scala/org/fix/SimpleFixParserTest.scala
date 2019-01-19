package org.fix

import org.scalatest.{FunSuite, Matchers}

class SimpleFixParserTest extends FunSuite with Matchers {
  val parser: SimpleFixParser = SimpleFixParser()
  test("parse null message") {
    parser.parse(FixSchema(), null) shouldBe FixMessage()
  }
  test("parse empty message") {
    parser.parse(FixSchema(), "") shouldBe FixMessage()
  }
  test("parse one field message no validation") {
    val fixSchema = FixSchema()
    parser.parse(fixSchema, s"35=0${FixMessage.fieldDelimiter}") shouldBe FixMessage(fixSchema, Seq(Field(35, "0")))
  }
}
