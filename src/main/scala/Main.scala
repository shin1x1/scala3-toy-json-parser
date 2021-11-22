import lexer.Scanner
import parser.Parser
import scala.io.StdIn

@main def main: Unit =
  def readAllText(): String =
    StdIn.readLine() match
      case null => ""
      case s => s + readAllText()

  val json = readAllText()
  val scanner = Scanner(json)

  println(Parser.parse(scanner))
