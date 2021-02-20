package org.fix

import org.fix.FixMessage.fixString

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.ListMap

trait FixField {
  val tag: Int
  val value: Any
}

case class Field(tag: Int, value: Any) extends FixField {
  private val data = s"$tag=$value"
  val length: Int = data.length + 1

  def fixString: String = data

  val isCalculated: Boolean = Field.calculatedFields.contains(tag)
}

object Field {
  val lengthTag = 9
  val checkSumTag = 10
  private val calculatedFields = List(lengthTag, checkSumTag)
}


case class Group(tag: Int, children: Seq[ListMap[Int, FixField]]) extends FixField {
  override val value: Any = children.size
}

object Group {
  def apply(tag: Int, children: List[List[FixField]]): Group = {
    new Group(tag, children.map { child =>
      ListMap(child.map(v => v.tag -> v): _*)
    })
  }
}

case class FixMessage(schema: FixSchema = FixSchema(), fields: ListMap[Int, FixField] = ListMap()) {
  def explain(indentation: Int = 4): List[String] = explain((f, d) => s"${" " * indentation * d}${f.tag} -> ${f.value}")

  def explain[T](af: (Field, Int) => T): List[T] = {
    @tailrec
    def ex(myFields: ListMap[Int, FixField], acc: List[T] = Nil, depth: Int = 0): List[T] = myFields.values.toList match {
      case Nil => acc
      case ff :: _ => ff match {
        case f: Field =>
          ex(myFields - f.tag, acc :+ af(f, depth), depth)
        case g: Group =>
          ex(myFields - g.tag, acc ++ (af(Field(g.tag, g.value), depth) +: exGroup(g.children.toList, Nil, depth + 1)))
      }
    }

    @tailrec
    def exGroup(children: List[ListMap[Int, FixField]], acc: List[T], depth: Int): List[T] = children match {
      case Nil => acc
      case c :: cs =>
        exGroup(cs, acc ++ ex(c, depth = depth), depth)
    }

    ex(fields)
  }

  def mkFixString(): String = fixString(this)
}

object FixMessage {
  val fieldDelimiter = "\u0001"

  def apply(schema: FixSchema, fields: List[FixField]): FixMessage = new FixMessage(schema, ListMap(fields.map(v => v.tag -> v): _*))

  def fixString(m: FixMessage): String = flattenFields(m).map(f => f.fixString).mkString(fieldDelimiter) + fieldDelimiter

  private def flattenFields(m: FixMessage): immutable.Seq[Field] = {
    @tailrec
    def _flatten(fields: List[FixField], acc: List[Field] = Nil): List[Field] = fields match {
      case Nil => acc
      case h :: tail => h match {
        case f: Field => _flatten(tail, acc :+ f)
        case g: Group => _flatten(g.children.flatMap(c => c.values).toList ++ tail, acc :+ Field(g.tag, g.value))
      }
    }

    _flatten(m.fields.values.toList)
  }
}
