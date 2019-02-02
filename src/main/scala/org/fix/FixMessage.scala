package org.fix

import scala.annotation.tailrec

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

case class FixMessage(schema: FixSchema = FixSchema(), fields: Map[Int, FixField] = Map()) {
  def explain(indentation: Int = 4): List[String] = explain((f, d) => s"${" " * indentation * d}${f.tag} -> ${f.value}")

  def explain[T](af: (Field, Int) => T): List[T] = {
    @tailrec
    def ex(myFields: Map[Int, FixField], acc: List[T] = Nil, depth: Int = 0): List[T] = myFields.values.toList match {
      case Nil => acc
      case ff :: _ => ff match {
        case f: Field =>
          ex(myFields - f.tag, acc :+ af(f, depth), depth)
        case g: Group =>
          ex(myFields - g.tag, acc ++ (af(Field(g.tag, g.value), depth) +: exGroup(g.children.toList, Nil, depth + 1)))
      }
    }

    @tailrec
    def exGroup(children: List[Map[Int, FixField]], acc: List[T], depth: Int): List[T] = children match {
      case Nil => acc
      case c :: cs =>
        exGroup(cs, acc ++ ex(c, depth = depth), depth)
    }

    ex(fields)
  }
}

object FixMessage {
  val fieldDelimiter = "\u0001"

  def apply(schema: FixSchema, fields: List[FixField]): FixMessage = new FixMessage(schema, fields.map(v => v.tag -> v).toMap)
}
