package org.fix

trait FixField {
  val tag: Int
  val value: Any
}

case class Field(tag: Int, value: Any) extends FixField

case class Group(tag: Int, children: Seq[Map[Int, FixField]]) extends FixField {
  override val value: Any = children.size
}

object Group {
  def apply(tag: Int, children: List[List[FixField]]): Group = new Group(tag, children.map(child => child.map(v => v.tag -> v).toMap))
}

case class FixMessage(schema: FixSchema = FixSchema(), fields: Map[Int, FixField] = Map())

object FixMessage {
  val fieldDelimiter = "\u0001"

  def apply(schema: FixSchema, fields: List[FixField]): FixMessage = new FixMessage(schema, fields.map(v => v.tag -> v).toMap)
}
