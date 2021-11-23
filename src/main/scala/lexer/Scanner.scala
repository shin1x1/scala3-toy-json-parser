package lexer

import java.io.{ByteArrayInputStream, InputStream}
import scala.annotation.tailrec

class Scanner(val inputStream: InputStream):
  private var position: Int = 0
  private var current: Option[Char] = read()

  def this(string: String) =
    this(ByteArrayInputStream(string.getBytes()))

  @tailrec
  private def read(): Option[Char] = inputStream.read() match
    case -1 => None
    case i => i.toChar match
      case c if c == ' ' || c == '\r' || c == '\n' || c == '\t' => read()
      case c => Some(c)

  def peek(): Option[Char] = current

  def consume(): Option[Char] =
    val ch = peek()
    current = read()
    ch

  def isEot(): Boolean = current.isEmpty
