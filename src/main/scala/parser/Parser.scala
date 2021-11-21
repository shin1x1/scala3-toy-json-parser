package parser

import lexer.{Scanner, Token, Lexer}

enum JsonValue:
  case Number(value: Double)
  case String(value: scala.Predef.String)
  case Null
  case True
  case False
  case Array(value: List[JsonValue])
  case Object(value: Map[scala.Predef.String, JsonValue])

type EitherJsonValue = Either[String, JsonValue]

object Parser:
  def parse(scanner: Scanner): EitherJsonValue =
    if scanner.isEot() then return Right(JsonValue.Null)

    Lexer.getNextToken(scanner) match
      case Right(t) => parseValue(t)(using scanner)
      case Left(e) => Left(e.toString)

  private def parseValue(token: Token)(using scanner: Scanner): EitherJsonValue = token match
    case Token.Number(v) => Right(JsonValue.Number(v))
    case Token.String(v) => Right(JsonValue.String(v))
    case Token.Null => Right(JsonValue.Null)
    case Token.True => Right(JsonValue.True)
    case Token.False => Right(JsonValue.False)
    case Token.LeftBrace => parseArray()
    case Token.LeftBracket => parseObject()
    case x => Left(s"Unexpected Token: $x")

  private def parseArray()(using scanner: Scanner): EitherJsonValue =
    enum State:
      case Default, Value, Comma

    import State.*

    def parse(state: State, list: List[JsonValue]): Option[List[JsonValue]] =
      val token = Lexer.getNextToken(scanner).getOrElse(return None)

      state match
        case Default => {
          token match
            case Token.RightBrace => return Some(list)
            case _ => {
              val v = parseValue(token).getOrElse(return None)
              parse(Value, list.appended(v))
            }
        }
        case Value => {
          token match
            case Token.RightBrace => return Some(list)
            case Token.Comma => parse(Comma, list)
            case _ => return None
        }
        case Comma => {
          val v = parseValue(token).getOrElse(return None)

          parse(Value, list.appended(v))
        }

    parse(Default, List()) match
      case Some(l) => Right(JsonValue.Array(l))
      case None => Left("Invalid array")

  private def parseObject()(using scanner: Scanner): EitherJsonValue =
    enum State:
      case Default, Value, Comma, Colon, Key

    import State.*

    def parse(state: State, map: Map[String, JsonValue], key: Option[String]): Option[Map[String, JsonValue]] =
      val token = Lexer.getNextToken(scanner).getOrElse(return None)

      state match
        case Default => token match
          case Token.RightBracket => Some(map)
          case Token.String(s) => parse(Key, map, Some(s))
          case _ => None
        case Key => token match
          case Token.Colon => parse(Colon, map, key)
          case _ => None
        case Colon => {
          val keyString = key.getOrElse(return None)
          parseValue(token) match
            case Right(v) => parse(Value, map + (keyString -> v), None)
            case Left(_) => None
        }
        case Value => token match
          case Token.RightBracket => Some(map)
          case Token.Comma => parse(Comma, map, None)
          case _ => None
        case Comma => token match
          case Token.RightBracket => return Some(map)
          case Token.String(s) => parse(Key, map, Some(s))
          case _ => None

    parse(Default, Map(), None) match
      case Some(l) => Right(JsonValue.Object(l))
      case None => Left("Invalid object")
