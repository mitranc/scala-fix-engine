package org.fix

trait FixParser {
  def parse(fix:String):FixMessage
}
case class SimpleFixParser() extends FixParser {
  override def parse(fix: String): FixMessage = {
    val emptyMessage = FixMessage(FixSchema(Seq()), Seq())
    Option(fix).fold(
      emptyMessage
    ) {
      case s if s.isEmpty => emptyMessage
      case _ => throw new UnsupportedOperationException("Not implemented yet")
    }

  }
}
