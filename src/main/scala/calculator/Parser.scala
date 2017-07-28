package calculator

import calculator.Tokenizer._

/**
  * Created by dawning on 2017/7/25.
  */

object Parser {

  sealed trait AST

  sealed trait Operator extends AST

  case object Plus extends Operator

  case object Minus extends Operator

  case object Multi extends Operator

  case object Divide extends Operator

  sealed trait Node extends AST

  case object Empty extends AST

  case class ExprNode(l: AST, r: AST) extends Node

  case class ExprTailNode(op: Operator, r: AST, left: AST) extends Node

  case class TermNode(l: AST, r: AST) extends Node

  case class TermTailNode(op: Operator, r: AST, left: AST) extends Node

  case class FactorNode(value: Double) extends AST

  /**
    * LL(1) grammar
    * Expr -> Expr + Term | Expr - Term | Term
    * Term -> Term * Factor | Term / Factor | Factor
    * Factor -> literal
    *
    * Expr -> Term Expr'
    * Expr' -> + Term Expr' | - Term Expr' | NULL
    * Term -> Factor Term'
    * Term' -> * Factor Term' | / Factor Term' | NULL
    * Factor -> literal
    *
    * @return
    */
  def expr(tks: List[Token]): (AST, List[Token]) = {
    val (termAST, xs1) = term(tks)
    val (exprTailAST, xs2) = exprTail(xs1)
    (ExprNode(termAST, exprTailAST), xs2)
  }

  def exprTail(tks: List[Token]): (AST, List[Token]) = tks match {
    case Tokenizer.Plus :: xs =>
      val (termAST, xs1) = term(xs)
      val (exprTailAST, xs2) = exprTail(xs1)
      (ExprTailNode(Plus, termAST, exprTailAST), xs2)
    case Tokenizer.Minus :: xs =>
      val (termAST, xs1) = term(xs)
      val (exprTailAST, xs2) = exprTail(xs1)
      (ExprTailNode(Minus, termAST, exprTailAST), xs2)
    case xs => (Empty, xs)
  }

  def term(tks: List[Token]): (AST, List[Token]) = {
    val (factorAST, xs1) = factor(tks)
    val (termTailAST, xs2) = termTail(xs1)
    (TermNode(factorAST, termTailAST), xs2)
  }

  def termTail(tks: List[Token]): (AST, List[Token]) = tks match {
    case Tokenizer.Multi :: xs =>
      val (factorAST, xs1) = factor(xs)
      val (termTailAST, xs2) = termTail(xs1)
      (TermTailNode(Multi, factorAST, termTailAST), xs2)
    case Tokenizer.Divide :: xs =>
      val (factorAST, xs1) = factor(xs)
      val (termTailAST, xs2) = termTail(xs1)
      (TermTailNode(Divide, factorAST, termTailAST), xs2)
    case xs => (Empty, xs)
  }

  def factor(tks: List[Token]): (AST, List[Token]) = tks match {
    case Literal(x) :: xs =>
      (FactorNode(x), xs)
    case x => throw new Error(s"excepted token $x")
  }

  def parse(tks: List[Token]): AST = expr(tks)._1

  def main(args: Array[String]): Unit = {
    println(parse(Tokenizer.read("1 + 2")))
  }
}
