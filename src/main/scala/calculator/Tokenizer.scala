package calculator

/**
  * Created by dawning on 2017/7/25.
  */

object Tokenizer {

  sealed trait Token

  case object Plus extends Token

  case object Minus extends Token

  case object Multi extends Token

  case object Divide extends Token

  case object Open extends Token

  case object Close extends Token

  case class Literal(v: Double) extends Token

  def read(in: String): List[Token] = readHelper(in.toList, new StringBuilder)

  def readHelper(in: List[Char], buffer: StringBuilder): List[Token] = in match {
    case x :: xs if isLiteral(x) => readHelper(xs, buffer + x)
    case x :: xs if isOpr(x) =>
      if (buffer.isEmpty) findOprToken(x) :: readHelper(xs, buffer)
      else Literal(buffer.toDouble) :: findOprToken(x) :: readHelper(xs, new StringBuilder)
    case x :: xs if isParentheses(x) =>
      if (buffer.isEmpty) findParenthesesToken(x) :: readHelper(xs, buffer)
      else Literal(buffer.toDouble) :: findParenthesesToken(x) :: readHelper(xs, new StringBuilder)
    case x :: xs if isWS(x) => readHelper(xs, buffer)
    case Nil => if (buffer.isEmpty) Nil else List(Literal(buffer.toDouble))
    case x :: _ => throw new UnknownError(s"excepted token [$x]")
  }

  def isLiteral(x: Char): Boolean = "0123456789." contains x

  def isOpr(x: Char): Boolean = "+-*/" contains x

  def isWS(x: Char): Boolean = " \t" contains x

  def isParentheses(x: Char): Boolean = "()" contains x

  def findOprToken(x: Char): Token = x match {
    case '+' => Plus
    case '-' => Minus
    case '*' => Multi
    case '/' => Divide
    case y => throw new UnknownError(s"excepted operator $y")
  }

  def findParenthesesToken(x: Char) = x match {
    case '(' => Open
    case ')' => Close
  }
}
