package org.fix

trait FixParser {
  def parse(schema: FixSchema, fix: String): FixMessage
}

case class SimpleFixParser() extends FixParser {
  override def parse(schema: FixSchema, fix: String): FixMessage = {
    FixMessage(schema, parse(schema, extractFields(fix)))
  }

  private def parse(schema: FixSchema, fields: Seq[FixField]): Seq[FixField] = fields
  private def extractFields(fix: String): Seq[FixField] = Option(fix)
    .fold(
      List[FixField]()
    )(m => if (m.isEmpty)
      List[FixField]()
    else
      m.split(FixMessage.fieldDelimiter)
        .map(_.split("="))
        .map((t: Array[String]) => Field(t(0).toInt, t(1))).toList)
}
