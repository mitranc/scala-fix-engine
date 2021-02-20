package org.fix

import org.scalactic.{Bad, Good}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SimpleFixParserTest extends AnyFunSuite with Matchers {
  val parser: SimpleFixParser = SimpleFixParser()
  test("parse null message") {
    parser.parse(FixSchema(), null) shouldBe Bad("FIX string cannot be null")
  }
  test("parse empty message") {
    parser.parse(FixSchema(), "") shouldBe Bad("FIX string cannot be empty")
  }
  test("parse one field message no validation") {
    val fixSchema = FixSchema()
    val inputFix = s"35=0${FixMessage.fieldDelimiter}"
    val Good(parsedMessage) = parser.parse(fixSchema, inputFix)
    parsedMessage shouldBe FixMessage(fixSchema, List(Field(35, "0")))
  }
  test("parse fields") {
    val fixSchema = FixSchema(
      List(MessageDef("0", "heartbeat",
        List(
          FieldDef(35, 'MsgType, "Message type"),
          FieldDef(9, 'BodyLength, "Body length")
        )))
    )
    val Good(parsedMessage) = parser.parse(fixSchema, s"35=0\0019=1\001")
    parsedMessage shouldBe FixMessage(fixSchema, List(Field(35, "0"), Field(9, "1")))
  }
  test("parse leniently when without matching message definition") {
    val fixSchema = FixSchema(List(
      MessageDef("0", "heartbeat",
        List(
          FieldDef(35, 'MsgType, "Message type"),
          FieldDef(9, 'BodyLength, "Body length")
        )
      )))
    val Good(parsedMessage) = parser.parse(fixSchema, s"35=R\0019=1\001")
    parsedMessage shouldBe FixMessage(fixSchema, List(Field(35, "R"), Field(9, "1")))
  }
  test("parse one element group") {
    val fixSchema = FixSchema(List(MessageDef("R", "Quote request",
      List(
        GroupDef(146, 'NoRelatedSym, "", mandatory = false,
          List(
            FieldDef(711, 'NoUnderlyings, ""),
            FieldDef(537, 'QuoteType, "")
          )),

      ))))
    val Good(message) = parser.parse(fixSchema, s"35=R\001146=1\001711=1\001537=0\001")
    message shouldBe FixMessage(
      fixSchema,
      List(Field(35, "R"),
        Group(146, List(
          List(
            Field(711, "1"),
            Field(537, "0")
          )
        ))
      )
    )
  }
  test("parse two element group") {
    val fixSchema = FixSchema(List(MessageDef("R", "Quote request",
      List(
        GroupDef(146, 'NoRelatedSym, "", mandatory = false,
          List(
            FieldDef(711, 'NoUnderlyings, ""),
            FieldDef(537, 'QuoteType, "")
          )),

      ))))
    val inputFix = s"35=R\001146=2\001711=1\001537=0\001711=3\001537=2\001555=SomeValue\001"
    val Good(parsedMessage) = parser.parse(fixSchema, inputFix)
    val expectedMessage = FixMessage(
      fixSchema,
      List(Field(35, "R"),
        Group(146, List(
          List(
            Field(711, "1"),
            Field(537, "0"),
          ),
          List(
            Field(711, "3"),
            Field(537, "2")
          ),
        )),
        Field(555, "SomeValue")
      )
    )
    println(expectedMessage.mkFixString())
    expectedMessage.mkFixString() shouldBe inputFix
    parsedMessage shouldBe expectedMessage
  }

  test("parse nested groups") {
    val fixSchema = FixSchema(List(MessageDef("R", "Quote request",
      List(
        GroupDef(146, 'NoRelatedSym, "", mandatory = false,
          List(
            FieldDef(711, 'NoUnderlyings, ""),
            FieldDef(537, 'QuoteType, ""),
            GroupDef(802, 'NoPartySubIDs, "", mandatory = false,
              List(
                FieldDef(523, 'PartySubID, ""),
                FieldDef(803, 'PartySubIDType, "")
              ))
          )
        ),
        FieldDef(555, 'SomeField, "")
      ))))
    val inputFix = s"35=R\001146=2\001711=1\001537=0\001802=2\001523=abc\001803=ABC\001523=bcd\001803=BCD\001711=2\001537=1\001802=2\001523=def\001803=DEF\001523=efg\001803=EFG\001555=SomeValue\001"
    val Good(parsedMessage) = parser.parse(fixSchema, inputFix)
    val expectedMessage = FixMessage(
      fixSchema,
      List(Field(35, "R"),
        Group(146,
          List(
            List(
              Field(711, "1"),
              Field(537, "0"),
              Group(802, List(
                List(
                  Field(523, "abc"),
                  Field(803, "ABC"),
                ),
                List(
                  Field(523, "bcd"),
                  Field(803, "BCD"),
                ),
              )),
            ),
            List(
              Field(711, "2"),
              Field(537, "1"),
              Group(802, List(
                List(
                  Field(523, "def"),
                  Field(803, "DEF"),
                ),
                List(
                  Field(523, "efg"),
                  Field(803, "EFG"),
                ),
              )),
            ),
          )),
        Field(555, "SomeValue"),
      )
    )
    println(expectedMessage.mkFixString())
    expectedMessage.mkFixString() shouldBe inputFix
    parsedMessage.mkFixString() shouldBe inputFix
    //    parsedMessage.mkFixString() shouldBe expectedMessage.mkFixString()
  }
}
