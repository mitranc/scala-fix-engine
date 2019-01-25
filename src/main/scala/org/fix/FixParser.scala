package org.fix

import scala.annotation.tailrec

trait FixParser {
  def parse(schema: FixSchema, fix: String): FixMessage
}

case class SimpleFixParser() extends FixParser {
  override def parse(schema: FixSchema, fix: String): FixMessage = new FixMessage(schema, parseFields(schema, extractFields(fix)))

  private def parseFields(schema: FixSchema, fields: List[Field]): Map[Int, FixField] = fields
    .find(f => f.tag == 35)
    .map(_.value.toString)
    .fold(
      fields.map(f => f.tag -> (f: FixField)).toMap
    ) { msgType =>
      schema.messageDefs.get(msgType)
        .fold(fields.map(f => f.tag -> (f: FixField)).toMap)(md => parse(md, fields))
    }

  private def parse(messageDef: MessageDef, fields: List[Field]): Map[Int, FixField] = parse(messageDef.partDefs, fields)._1

  @tailrec
  private def parse(partDefs: Map[Int, PartDef], fields: List[Field], acc: Map[Int, FixField] = Map(), fieldCount: Int = 0): (Map[Int, FixField], Int) = fields match {
    case Nil => (acc, fieldCount)
    case ff :: ffs =>
      partDefs.get(ff.tag) match {
        case Some(partDef) =>
          val restDefs = partDefs - ff.tag
          partDef match {
            //fields are just added to the model
            case _: FieldDef =>
              parse(restDefs, ffs, acc + (ff.tag -> ff), fieldCount + 1)
            //groups need to be converted to group
            case gd: GroupDef =>
              val (group, groupFieldCount) = parseGroup(ff, gd, ffs)
              parse(restDefs, ffs.drop(groupFieldCount), acc + (group.tag -> group), groupFieldCount)
          }
        // lenient parse - if no schema part def then add the field anyway
        case None => parse(partDefs, ffs, acc + (ff.tag -> ff), fieldCount + 1)
      }
  }

  private def parseGroup(f: FixField, groupDef: GroupDef, restFields: List[Field], fieldCount: Int = 0): (Group, Int) = {
    val subGroupDefs: List[Map[Int, PartDef]] = for (_ <- List(1 to f.value.toString.toInt)) yield groupDef.childrenDefs
    val (subGroups, subGroupsFieldCount) = parseSubGroups(subGroupDefs, restFields)
    (Group(f.tag, subGroups), subGroupsFieldCount + 1)
  }

  @tailrec
  private def parseSubGroups(
                              groupPartDefs: List[Map[Int, PartDef]],
                              restFields: List[Field],
                              acc: List[Map[Int, FixField]] = Nil,
                              fieldCount: Int = 0): (List[Map[Int, FixField]], Int) = groupPartDefs match {
    case Nil => (acc, fieldCount)
    case gpd :: gpds =>
      val (subGroup, subGroupFieldCount) = parse(gpd, restFields)
      parseSubGroups(gpds, restFields.drop(subGroupFieldCount), subGroup +: acc, fieldCount + subGroupFieldCount)
  }

  private def extractFields(fix: String): List[Field] = Option(fix)
    .fold(
      List[Field]()
    )(m => if (m.isEmpty)
      List[Field]()
    else
      m.split(FixMessage.fieldDelimiter)
        .map(_.split("="))
        .map((t: Array[String]) => Field(t(0).toInt, t(1))).toList)
}
