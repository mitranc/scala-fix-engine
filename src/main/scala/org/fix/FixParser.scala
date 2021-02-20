package org.fix

import org.scalactic.OptionSugar.Optionizer
import org.scalactic.{Bad, ErrorMessage, Good, Or}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.Try

trait FixParser {
  def parse(schema: FixSchema, fix: String): FixMessage Or ErrorMessage
}

case class SimpleFixParser() extends FixParser {
  override def parse(schema: FixSchema, fix: String): FixMessage Or ErrorMessage = for {
    fix <- Option(fix) toOr "FIX string cannot be null"
    _ <- if (fix.isEmpty) Bad("FIX string cannot be empty") else Good().orBad[ErrorMessage]
    _ <- if (!fix.endsWith(FixMessage.fieldDelimiter)) Bad(s"FIX string must end with field delimiter ${FixMessage.fieldDelimiter}") else Good().orBad[ErrorMessage]
    fields <- extractFields(fix)
    m <- parseFields(schema, fields)
  } yield new FixMessage(schema, m)


  private def parseFields(schema: FixSchema, fields: List[Field]): ListMap[Int, FixField] Or ErrorMessage = Good(fields
    .find(f => f.tag == 35)
    .map(_.value.toString)
    .fold(
      ListMap(fields.map(f => f.tag -> (f: FixField)): _*)
    ) { msgType =>
      schema.messageDefs.get(msgType)
        .fold(ListMap(fields.map(f => f.tag -> (f: FixField)): _*))(md => parse(md, fields))
    })

  private def parse(messageDef: MessageDef, fields: List[Field]): ListMap[Int, FixField] = parse(messageDef.partDefs, fields)._1

  @tailrec
  private def parse(
                     partDefs: Map[Int, PartDef],
                     fields: List[Field],
                     isSubGroup: Boolean = false,
                     acc: ListMap[Int, FixField] = ListMap(),
                     fieldCount: Int = 0
                   ): (ListMap[Int, FixField], Int) = fields match {
    case Nil =>
      (acc, fieldCount)
    case fixField :: restFields =>
      partDefs.get(fixField.tag) match {
        case Some(partDef) =>
          val restDefs = partDefs - fixField.tag
          partDef match {
            //fields are just added to the model
            case _: FieldDef =>
              parse(restDefs, restFields, isSubGroup, acc + (fixField.tag -> fixField), fieldCount + 1)
            //groups need to be converted to group
            case gd: GroupDef =>
              val (group, groupFieldCount) = parseGroup(fixField, gd, restFields)
              val newRestFields = restFields.drop(groupFieldCount - 1)
              parse(restDefs, newRestFields, isSubGroup, acc + (group.tag -> group), fieldCount + groupFieldCount)
          }
        case None =>
          if (isSubGroup) {
            //end of subgroup return
            (acc, fieldCount)
          }
          else {
            // lenient parse - if no schema part def then add the field anyway
            parse(partDefs, restFields, isSubGroup, acc + (fixField.tag -> fixField), fieldCount + 1)
          }
      }
  }

  private def parseGroup(f: FixField, groupDef: GroupDef, restFields: List[Field]): (Group, Int) = {
    val groupChildrenCount = f.value.toString.toInt
    val subGroupDefs: ListMap[Int, PartDef] = groupDef.childrenDefs
    val (subGroups, subGroupsFieldCount) = parseSubGroups(subGroupDefs, restFields, groupChildrenCount)
    (Group(f.tag, subGroups), subGroupsFieldCount + 1)
  }

  @tailrec
  private def parseSubGroups(
                              childrenDefs: ListMap[Int, PartDef],
                              restFields: List[Field],
                              groupChildrenCount: Int,
                              acc: List[ListMap[Int, FixField]] = Nil,
                              fieldCount: Int = 0
                            ): (List[ListMap[Int, FixField]], Int) =
    if (groupChildrenCount <= 0) {
      (acc, fieldCount)
    } else {
      val (subGroup, subGroupFieldCount) = parse(childrenDefs, restFields, isSubGroup = true)
      val newRestFields = restFields.drop(subGroupFieldCount)
      parseSubGroups(
        childrenDefs,
        newRestFields,
        groupChildrenCount - 1,
        acc :+ subGroup,
        fieldCount + subGroupFieldCount
      )
    }

  private def extractFields(fix: String): List[Field] Or ErrorMessage = {
    fix.split(FixMessage.fieldDelimiter).toList match {
      case Nil => Bad("FIX message should contain minimum one field")
      case some => extractFields(some)
    }
  }

  private def extractFields(stringFields: List[String]): List[Field] Or ErrorMessage = {
    @tailrec
    def _extract(rawFields: List[String], acc: List[Field] = Nil): List[Field] Or ErrorMessage = rawFields match {
      case Nil => Good(acc)
      case h :: tail => extractField(h) match {
        case Bad(b) => Bad(b)
        case Good(field) => _extract(tail, acc :+ field)
      }
    }

    _extract(stringFields)
  }

  private def extractField(str: String): Field Or ErrorMessage = {
    val split = str.split("=")
    if (split.length == 2) {
      val tagString = split(0)
      for {
        tag <- Or.from(Try {
          val tagSplit = tagString.split("-")
          val trimTag = tagSplit(0).trim
          trimTag.toInt
        }).badMap(_ => s"Could not parse tag $tagString")
        value = split(1)
      } yield Field(tag, value)
    } else Bad(s"Field $str must have separator =")
  }
}