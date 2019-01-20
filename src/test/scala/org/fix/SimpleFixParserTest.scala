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
    parser.parse(fixSchema, s"35=0${FixMessage.fieldDelimiter}").fields shouldBe List(Field(35, "0"))
  }
  test("parse fields") {
    val fixSchema = FixSchema(Seq(MessageDef("0", "heartbeat", List(
      FieldDef(35, 'MsgType, "Message type"),
      FieldDef(9, 'BodyLength, "Body length")
    ))))
    parser.parse(fixSchema, s"35=0\0019=1\001").fields shouldBe List(Field(35, "0"), Field(9, "1"))
  }
  test("parse leniently when without matching message definition") {
    val fixSchema = FixSchema(Seq(MessageDef("0", "heartbeat", List(
      FieldDef(35, 'MsgType, "Message type"),
      FieldDef(9, 'BodyLength, "Body length")
    ))))
    parser.parse(fixSchema, s"35=R\0019=1\001").fields shouldBe List(Field(35, "R"), Field(9, "1"))
  }
  test("parse one element group") {
    val fixSchema = FixSchema(Seq(MessageDef("R", "Quote request", List(
      GroupDef(146, 'NoRelatedSym, "", mandatory = false,
        List(
          FieldDef(711, 'NoUnderlyings, ""),
          FieldDef(537, 'QuoteType, "")
        )),

    ))))
    val message = parser.parse(fixSchema, s"35=R\001146=1\001711=1\001537=0")
    message.fields shouldBe Seq(
      Field(35, "R"),
      Group(146, List(
        List(Field(711, "1"), Field(537, "0"))
      )))
  }
}
