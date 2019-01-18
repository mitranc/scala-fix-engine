package org.fix

import org.scalatest.{FunSuite, Matchers}

class SimpleFixParserTest extends FunSuite with Matchers {
  val parser: SimpleFixParser = SimpleFixParser()
  test("parse null message") {
    parser.parse(null) shouldBe FixMessage()
  }
  test("parse empty message") {
    parser.parse("") shouldBe FixMessage()
  }

}
