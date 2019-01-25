package org.fix

import scala.annotation.tailrec

case class FixSchema(messageDefs: Map[String, MessageDef] = Map()) {
  val symbolTags: Map[Symbol, Int] = messageDefs.values.map(_.partDefs).flatMap(symbolTags(_)).toMap
  val tagSymbols: Map[Int, Symbol] = symbolTags.map(identity(_).swap)

  @tailrec
  private def symbolTags(partDefs: Map[Int, PartDef], accumulator: Seq[(Symbol, Int)] = Nil): Seq[(Symbol, Int)] = {
    partDefs.values.toList match {
      case Nil => accumulator
      case h :: _ =>
        h match {
          case f: FieldDef => symbolTags(partDefs - f.tag, accumulator :+ (f.symbol, f.tag))
          case g: GroupDef => symbolTags(g.childrenDefs ++ partDefs - g.tag, accumulator :+ (g.symbol, g.tag))
        }
    }
  }
}

object FixSchema {
  def apply(messageDefs: List[MessageDef]): FixSchema = new FixSchema(messageDefs.map(v => v.msgType -> v).toMap)
}

trait PartDef {
  val tag: Int
  val symbol: Symbol
  val description: String
  val mandatory: Boolean
}

case class FieldDef(
                     tag: Int,
                     symbol: Symbol,
                     description: String,
                     mandatory: Boolean = false,
                   ) extends PartDef

case class GroupDef(
                     tag: Int,
                     symbol: Symbol,
                     description: String,
                     mandatory: Boolean,
                     childrenDefs: Map[Int, PartDef]
                   ) extends PartDef

object GroupDef {
  def apply(
             tag: Int,
             symbol: Symbol,
             description: String,
             mandatory: Boolean,
             childrenDefs: List[PartDef]
           ): GroupDef = new GroupDef(tag, symbol, description, mandatory, childrenDefs.map(v => v.tag -> v).toMap)
}

case class MessageDef(msgType: String, description: String, partDefs: Map[Int, PartDef])

object MessageDef {
  val sessionFixHeader: List[FieldDef] = List(
    FieldDef(8, 'BeginString, "", mandatory = true),
    FieldDef(9, 'BodyLength, "", mandatory = true),
    FieldDef(35, 'MsgType, "", mandatory = true),
    FieldDef(34, 'MsgSeqNum, "", mandatory = true),
    FieldDef(49, 'SenderCompID, "", mandatory = true),
    FieldDef(52, 'SendingTime, "", mandatory = true),
    FieldDef(59, 'TargetCompID, "", mandatory = true),
  )
  val sessionFixTrailer: Seq[FieldDef] = Seq(
    FieldDef(10, 'CheckSum, "", mandatory = true),
  )

  def apply(msgType: String, description: String, partDefs: List[PartDef]): MessageDef = MessageDef(msgType, description, partDefs.map(v => v.tag -> v).toMap)
}