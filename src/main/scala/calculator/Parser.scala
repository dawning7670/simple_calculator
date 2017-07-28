package calculator

import calculator.Tokenizer._

/**
  * Created by dawning on 2017/7/25.
  */

/**
  * LL(1) Parser
  * Expr -> Expr + Term | Expr - Term | Term
  * Term -> Term * Factor | Term / Factor | Factor
  * Factor -> literal  | (Expr)
  *
  * Expr -> Term Expr'
  * Expr' -> + Term Expr' | - Term Expr' | NULL
  * Term -> Factor Term'
  * Term' -> * Factor Term' | / Factor Term' | NULL
  * Factor -> literal | (Expr)
  *
  * @return
  */
object Parser {

  sealed trait AST

  case object Empty extends AST

  case object Plus extends AST

  case object Minus extends AST

  case object Multi extends AST

  case object Divide extends AST

  case class Expr(l: AST, op: AST, r: AST) extends AST

  case class Term(l: AST, op: AST, r: AST) extends AST

  case class Factor(value: Double) extends AST

  def expr(tks: List[Token]): (AST, List[Token]) = {
    val (termAST, xs) = term(tks)
    exprTail(xs) match {
      case (Empty, xs) => (Expr(termAST, Empty, Empty), xs)
      case (op, xs) => val (exprAST, xs1) = expr(xs)
        (Expr(termAST, op, exprAST), xs1)
    }
  }

  def exprTail(tks: List[Token]): (AST, List[Token]) = tks match {
    case Tokenizer.Plus :: xs => (Plus, xs)
    case Tokenizer.Minus :: xs => (Minus, xs)
    case _ => (Empty, tks)
  }

  def term(tks: List[Token]): (AST, List[Token]) = {
    val (factorAST, xs1) = factor(tks)
    termTail(xs1) match {
      case (Empty, xs) => (Term(factorAST, Empty, Empty), xs)
      case (op, xs) => val (termAST, xs1) = term(xs)
        (Term(factorAST, op, termAST), xs1)
    }
  }

  def termTail(tks: List[Token]): (AST, List[Token]) = tks match {
    case Tokenizer.Multi :: xs => (Multi, xs)
    case Tokenizer.Divide :: xs => (Divide, xs)
    case _ => (Empty, tks)
  }

  def factor(tks: List[Token]): (AST, List[Token]) = tks match {
    case Literal(x) :: xs => (Factor(x), xs)
    case Open :: xs =>
      val closePos = xs indexOf Close
      require(closePos != -1, s"parentheses match error. $tks")
      val (ls, rs) = xs.splitAt(xs indexOf Close)
      (expr(ls)._1, rs.tail)
    case _ => throw new Error(s"excepted token $tks")
  }

  def parse(tks: List[Token]): AST = expr(tks)._1
}
