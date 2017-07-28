package calculator

import calculator.Parser._

/**
  * Created by dawning on 2017/7/25.
  */
object Evaluator {
  def eval(ast: AST): Double = ast match {
    case ExprNode(l, r) => eval(l) + eval(r)
    case ExprTailNode(Plus, r, left) => eval(r)
    case ExprTailNode(Minus, r, left) => eval(r)
  }
}
