package org.fix

trait FixField {
  val tag: Int
  val value: Any
}

case class Field(tag: Int, value: Any) extends FixField

case class Group(tag: Int, children: Seq[Seq[FixField]]) extends FixField {
  override val value: Any = children.size
}

case class FixMessage(schema: FixSchema = FixSchema(), fields: List[FixField] = Nil)

object FixMessage {
  val fieldDelimiter = "\001"
}
