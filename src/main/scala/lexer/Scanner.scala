package lexer

case class Scanner(val json: String):
  var position: Int = 0

  def peek(): Option[Char] =
    if position < json.length then
      Some(json.charAt(position))
    else
      None

  def consume(): Option[Char] =
    val ch = peek()
    if ch.isDefined then position = position + 1
    ch

  def isEot(): Boolean = peek().isEmpty
