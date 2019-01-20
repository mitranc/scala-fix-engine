package org.fix

import scala.annotation.tailrec

trait FixParser {
  def parse(schema: FixSchema, fix: String): FixMessage
}

case class SimpleFixParser() extends FixParser {
  override def parse(schema: FixSchema, fix: String): FixMessage = FixMessage(schema, parseFields(schema, extractFields(fix)))

  private def parseFields(schema: FixSchema, fields: List[FixField]): List[FixField] = fields
    .find(f => f.tag == 35)
    .map(_.value.toString)
    .fold(
      fields
    ) { msgType =>
      schema.messageDefs
        .find(md => md.msgType == msgType)
        .fold(fields)(md => parse(md, fields))
    }

  private def parse(messageDef: MessageDef, fields: List[FixField]): List[FixField] = parse(messageDef.partDefs, fields)._1

  @tailrec
  private def parse(partDefs: List[PartDef], fields: List[FixField], acc: List[FixField] = Nil, fieldCount: Int = 0): (List[FixField], Int) = fields match {
    case Nil => (acc, fieldCount)
    case ff :: ffs =>
      partDefs.find(pd => pd.tag == ff.tag) match {
        case Some(partDef) =>
          val restDefs = partDefs.filterNot(pd => pd.tag == ff.tag)
          partDef match {
            //fields are just added to the model
            case _: FieldDef =>
              parse(restDefs, ffs, acc :+ ff, fieldCount + 1)
            //groups need to be converted to group
            case gd: GroupDef =>
              val (group, groupFieldCount) = parseGroup(ff, gd, ffs)
              parse(restDefs, ffs.drop(groupFieldCount), acc :+ group, groupFieldCount)
          }
        // lenient parse - if no schema part def then add the field anyway
        case None => parse(partDefs, ffs, acc :+ ff, fieldCount + 1)
      }
  }

  private def parseGroup(f: FixField, groupDef: GroupDef, restFields: List[FixField], fieldCount: Int = 0): (Group, Int) = {
    val subGroupDefs: List[List[PartDef]] = for (_ <- List(1 to f.value.toString.toInt)) yield groupDef.childrenDefs
    val (subGroups, subGroupsFieldCount) = parseSubGroups(subGroupDefs, restFields)
    (Group(f.tag, subGroups), subGroupsFieldCount + 1)
  }

  @tailrec
  private def parseSubGroups(
                              groupPartDefs: List[List[PartDef]],
                              restFields: List[FixField],
                              acc: List[List[FixField]] = Nil,
                              fieldCount: Int = 0): (List[List[FixField]], Int) = groupPartDefs match {
    case Nil => (acc, fieldCount)
    case gpd :: gpds =>
      val (subGroup, subGroupFieldCount) = parse(gpd, restFields)
      parseSubGroups(gpds, restFields.drop(subGroupFieldCount), subGroup +: acc, fieldCount + subGroupFieldCount)
  }

  private def extractFields(fix: String): List[FixField] = Option(fix)
    .fold(
      List[FixField]()
    )(m => if (m.isEmpty)
      List[FixField]()
    else
      m.split(FixMessage.fieldDelimiter)
        .map(_.split("="))
        .map((t: Array[String]) => Field(t(0).toInt, t(1))).toList)
}
