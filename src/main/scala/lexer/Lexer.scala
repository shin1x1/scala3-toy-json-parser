package lexer

import scala.annotation.tailrec
import scala.math.pow

enum Token:
  case LeftBracket, RightBracket
  case LeftBrace, RightBrace
  case Colon, Comma
  case Null, True, False
  case Number(value: Double)
  case String(value: scala.Predef.String)

class InvalidCharacter(val char: Char) extends Exception

class InvalidCodepoint() extends Exception

class InvalidLiteralCharacter() extends Exception

type EitherToken = Either[Exception, Token]

object Lexer:
  def getNextToken(scanner: Scanner): EitherToken = scanner.consume() match
    case Some(c) if c == ' ' || c == '\r' || c == '\n' || c == '\t' => getNextToken(scanner)
    case Some(c) => lex(c)(using scanner)
    case None => Left(IndexOutOfBoundsException())

  private def lex(char: Char)(using Scanner): EitherToken = char match
    case '[' => Right(Token.LeftBrace)
    case ']' => Right(Token.RightBrace)
    case '{' => Right(Token.LeftBracket)
    case '}' => Right(Token.RightBracket)
    case ':' => Right(Token.Colon)
    case ',' => Right(Token.Comma)
    case '-' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => lexNumber(char)
    case '"' => lexString(char)
    case 't' => lexLiteral("true", Token.True)
    case 'f' => lexLiteral("false", Token.False)
    case 'n' => lexLiteral("null", Token.Null)
    case x => Left(InvalidCharacter(x))

  private def lexNumber(char: Char)(using scanner: Scanner): EitherToken =
    @tailrec
    def lex(string: String): String = scanner.peek() match
      case Some(c) if "-+.e0123456789".contains(c) => {
        scanner.consume()
        lex(string :+ c)
      }
      case _ => string

    try
      Right(Token.Number(lex(char.toString).toDouble))
    catch
      case e: Exception => Left(e)

  private def lexString(char: Char)(using scanner: Scanner): EitherToken =
    def lexCodepoint(): Option[Char] =
      val isHex = (c: Char) => "0123456789ABCDEF".contains(c)
      for
        h1 <- scanner.consume().filter(isHex)
        h2 <- scanner.consume().filter(isHex)
        h3 <- scanner.consume().filter(isHex)
        h4 <- scanner.consume().filter(isHex)
      yield
        ((h1 - '0' << 12) + (h2 - '0' << 8) + (h3 - '0' << 4) + h4 - '0').toChar

    @tailrec
    def lex(string: String, backslash: Boolean = false): EitherToken = scanner.consume() match
      case Some(s) if backslash => {
        val ch = s match
          case '"' => '"'
          case '\\' => '\\'
          case '/' => '/'
          case 'b' => '\b'
          case 'f' => '\f'
          case 'n' => '\n'
          case 'r' => '\r'
          case 't' => '\t'
          case 'u' => lexCodepoint() match
            case Some(c1) => c1
            case None => return Left(InvalidCodepoint())
          case _ => '?'

        lex(string :+ ch, false)
      }
      case Some(s) if s == '"' => Right(Token.String(string))
      case Some(s) if s == '\\' => lex(string, true)
      case Some(s) => lex(string :+ s)
      case None => Left(Exception())

    lex("")

  private def lexLiteral(literal: String, token: Token)(using scanner: Scanner): EitherToken =
    literal.substring(1).foreach(c => scanner.consume() match
      case Some(s) if s == c => ()
      case _ => return Left(InvalidLiteralCharacter())
    )

    Right(token)


