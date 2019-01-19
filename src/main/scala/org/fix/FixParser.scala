package org.fix

trait FixParser {
  def parse(schema: FixSchema, fix: String): FixMessage
}

case class SimpleFixParser() extends FixParser {
  override def parse(schema: FixSchema, fix: String): FixMessage = {
    val emptyMessage = FixMessage(FixSchema(Seq()), Seq())
    Option(fix)
      .fold(
        emptyMessage
      ) {
        case s if s.isEmpty => emptyMessage
        case m => FixMessage(schema, extractFields(m))
      }
  }

  private def extractFields(fix: String): Seq[FixField] = fix
    .split(FixMessage.fieldDelimiter)
    .map(_.split("="))
    .map((t: Array[String]) => Field(t(0).toInt, t(1))).toList
}
