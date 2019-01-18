package org.fix

case class FixSchema(messageDefs: Seq[MessageDef] = Seq())

trait PartDef {
  val tag: Int
  val symbol: Symbol
  val description: String
  val mandatory: Boolean
}

case class FieldDef(
                     tag: Int,
                     symbol: Symbol,
                     description: String,
                     mandatory: Boolean,
                   ) extends PartDef

case class GroupDef(
                     tag: Int,
                     symbol: Symbol,
                     description: String,
                     mandatory: Boolean,
                     childrenDefs: Seq[FieldDef]
                   ) extends PartDef

case class MessageDef(partDefs: Seq[PartDef])