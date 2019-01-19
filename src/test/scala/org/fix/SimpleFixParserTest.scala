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
  test("parse fields") {
    val fixSchema = FixSchema(Seq(MessageDef("0", "heartbeat", Seq(
      FieldDef(35, 'MsgType, "Message type"),
      FieldDef(9, 'BodyLength, "Body length")
    ))))
    parser.parse(fixSchema, s"35=0\0019=1\001") shouldBe FixMessage(fixSchema, Seq(Field(35, "0"), Field(9, "1")))
  }
  test("parse leniently when without matching message definition") {
    val fixSchema = FixSchema(Seq(MessageDef("0", "heartbeat", Seq(
      FieldDef(35, 'MsgType, "Message type"),
      FieldDef(9, 'BodyLength, "Body length")
    ))))
    parser.parse(fixSchema, s"35=R\0019=1\001") shouldBe FixMessage(fixSchema, Seq(Field(35, "R"), Field(9, "1")))
  }
  ignore("parse one element group") {
    val fixSchema = FixSchema(Seq(MessageDef("R", "Quote request", Seq(
      GroupDef(146, 'NoRelatedSym, "", mandatory = false,
        Seq(
          FieldDef(711, 'NoUnderlyings, ""),
          FieldDef(537, 'QuoteType, "")
        )),

    ))))
    parser.parse(fixSchema, s"146=1\001711=1\001537=0") shouldBe FixMessage(fixSchema, Seq(Group(35, Seq(
      Seq(Field(711, "1"), Field(537, "0"))
    ))))
  }
}
