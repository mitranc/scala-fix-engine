package org.fix

import scala.annotation.tailrec

case class FixSchema(messageDefs: Seq[MessageDef] = Seq()) {
  val symbolTags: Map[Symbol, Int] = messageDefs.map(_.partDefs).flatMap(symbolTags(_)).toMap
  val tagSymbols: Map[Int, Symbol] = symbolTags.map(identity(_).swap)

  @tailrec
  private def symbolTags(partDefs: Seq[PartDef], accumulator: Seq[(Symbol, Int)] = Nil): Seq[(Symbol, Int)] = partDefs match {
    case Nil => accumulator
    case h :: tail => h match {
      case f: FieldDef => symbolTags(tail, accumulator :+ (f.symbol, f.tag))
      case g: GroupDef => symbolTags(g.childrenDefs ++ tail, accumulator :+ (g.symbol, g.tag))
    }
  }
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
                     childrenDefs: Seq[FieldDef]
                   ) extends PartDef

case class MessageDef(msgType: String, description: String, partDefs: Seq[PartDef])

object MessageDef {
  val sessionFixHeader: Seq[FieldDef] = Seq(
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
}