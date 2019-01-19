package org.fix

import org.scalatest.{FunSuite, Matchers}

class FixSchemaTest extends FunSuite with Matchers {
  test("generate symbol map for empty message") {
    val schema = FixSchema()
    schema.tagSymbols shouldBe empty
    schema.symbolTags shouldBe empty
  }
  test("generate symbol map for fields and group definition mixtures") {
    val fixSchema = FixSchema(Seq(
      MessageDef("R", "Fictive quote request", Seq(
        FieldDef(1, 'Account, "Customer account"),
        GroupDef(146, 'NoRelSym, "Number related symbols", mandatory = false, Seq(
          FieldDef(54, 'Side, "Side of the trade"),
          FieldDef(38, 'OrderQty, "Order Quantity")
        )),
        FieldDef(58, 'Text, "Text"),
        GroupDef(555, 'NoLegs, "", mandatory = false, Seq(
          FieldDef(687, 'LegQty, "Leg quantity")
        ))
      ))
    ))
    fixSchema.symbolTags shouldBe Map(
      'Account -> 1,
      'NoRelSym -> 146,
      'Side -> 54,
      'OrderQty -> 38,
      'Text -> 58,
      'NoLegs -> 555,
      'LegQty -> 687
    )
    fixSchema.tagSymbols shouldBe Map(
      1 -> 'Account,
      146 -> 'NoRelSym,
      54 -> 'Side,
      38 -> 'OrderQty,
      58 -> 'Text,
      555 -> 'NoLegs,
      687 -> 'LegQty
    )
  }

}
