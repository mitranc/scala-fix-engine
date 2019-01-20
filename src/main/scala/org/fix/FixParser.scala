package org.fix

import scala.annotation.tailrec

trait FixParser {
  def parse(schema: FixSchema, fix: String): FixMessage
}

case class SimpleFixParser() extends FixParser {
  override def parse(schema: FixSchema, fix: String): FixMessage = {
    FixMessage(schema, parseFields(schema, extractFields(fix)))
  }

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

  private def parse(messageDef: MessageDef, fields: List[FixField]): List[FixField] = parse(messageDef.partDefs, fields)

  private def parse(partDefs: List[PartDef], fields: List[FixField]): List[FixField] = fields match {
    case Nil => Nil
    case ff :: ffs =>
      partDefs
        .find(pd => pd.tag == ff.tag)
        .fold(
          // lenient parse - if no schema part def then add the field anyway
          ff +: parse(partDefs.filterNot(pd => pd.tag == ff.tag), ffs)
        ) {
          //fields are just added to the model
          case fd: FieldDef =>
            ff +: parse(partDefs.filterNot(pd => pd.tag == ff.tag), ffs)
          //groups need to be converted to group
          case gd: GroupDef =>
            val group = parseGroup(ff, gd, ffs)
            val restDefs = partDefs.filterNot(pd => pd.tag == gd.tag)
            val restFields = ffs.drop(countFields(group.children.flatten))
            group +: parse(restDefs, restFields)
        }
  }

  def parseGroup(f: FixField, groupDef: GroupDef, restFields: List[FixField]): Group = {
    val subGroupDefs: List[List[PartDef]] = for (_ <- List(1 to f.value.toString.toInt)) yield groupDef.childrenDefs
    val subGroups = parseSubGroups(subGroupDefs, restFields)
    Group(f.tag, subGroups)
  }

  @tailrec
  private def parseSubGroups(groupPartDefs: List[List[PartDef]], restFields: List[FixField], acc: List[List[FixField]] = Nil): List[List[FixField]] = groupPartDefs match {
    case Nil => acc
    case gpd :: gpds =>
      val subGroup = parse(gpd, restFields)
      parseSubGroups(gpds, restFields.drop(countFields(subGroup)), subGroup +: acc)
  }

  @tailrec
  private def countFields(fixFields: Seq[FixField], counter: Int = 0): Int = fixFields match {
    case Nil => counter
    case ff :: ffs => ff match {
      case f: Field => countFields(ffs, counter + 1)
      case g: Group => countFields(g.children.flatten ++ ffs, counter + 1)
    }
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
