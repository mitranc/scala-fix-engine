package org.fix

trait FixParser {
  def parse(schema: FixSchema, fix: String): FixMessage
}

case class SimpleFixParser() extends FixParser {
  override def parse(schema: FixSchema, fix: String): FixMessage = {
    FixMessage(schema, parseFields(schema, extractFields(fix)))
  }

  private def parseFields(schema: FixSchema, fields: Seq[FixField]): Seq[FixField] = fields
    .find(f => f.tag == 35)
    .map(_.value.toString)
    .fold(
      fields
    ) { msgType =>
      schema.messageDefs
        .find(md => md.msgType == msgType)
        .fold(fields)(md => parse(md, fields))
    }

  private def parse(messageDef: MessageDef, fields: Seq[FixField]): Seq[FixField] = parse(messageDef.partDefs, fields)

  private def parse(partDefs: Seq[PartDef], fields: Seq[FixField]): Seq[FixField] = fields

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
