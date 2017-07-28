package calculator

import calculator.Parser._

/**
  * Created by dawning on 2017/7/25.
  */
object Evaluator {
  def eval(ast: AST): Double = ast match {
    case Expr(l, Plus, r) => eval(l) + eval(r)
    case Expr(l, Minus, r) => eval(l) - eval(r)
    case Expr(l, Empty, _) => eval(l)
    case Term(l, Multi, r) => eval(l) * eval(r)
    case Term(l, Divide, r) => eval(l) / eval(r)
    case Term(l, Empty, _) => eval(l)
    case Factor(x) => x
    case _ => throw new IllegalArgumentException(s"evaluate error. Invalid AST: [$ast]")
  }
}
