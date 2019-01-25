package org.fix

trait FixField {
  val tag: Int
  val value: Any
}

case class Field(tag: Int, value: Any) extends FixField

case class Group(tag: Int, children: Seq[Map[Int, FixField]]) extends FixField {
  override val value: Any = children.size
}

case class FixMessage(schema: FixSchema = FixSchema(), fields: Map[Int, FixField] = Map())

object FixMessage {
  val fieldDelimiter = "\001"
}
