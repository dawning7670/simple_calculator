import calculator._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by dawning on 2017/7/25.
  */
class ParserTest extends FlatSpec {

  import calculator.Parser._

  def buildAST(in: String): AST = {
    parse(Tokenizer.read(in))
  }

  "[1 + 2]" should "equal Expr(Term(Factor(1), Empty, Empty),Plus,Expr(Term(Factor(2), Empty, Empty), Empty, Empty))" in {
    buildAST("1 + 2") should equal(Expr(
      Term(Factor(1), Empty, Empty),
      Plus,
      Expr(Term(Factor(2), Empty, Empty), Empty, Empty)))
  }

  "1 * 2 - 3" should "equal Expr(Term(Factor(1),Multi,Term(Factor(2), Empty, Empty))," +
    "Minus,Expr(Term(Factor(3), Empty, Empty),Empty,Empty))" in {
    buildAST("1 * 2 - 3") should equal(Expr(
      Term(Factor(1), Multi, Term(Factor(2), Empty, Empty)),
      Minus,
      Expr(
        Term(Factor(3), Empty, Empty),
        Empty,
        Empty)))
  }

  "(1)" should "equal Expr(Term(Expr(Term(Factor(1), Empty, Empty), Empty, Empty), Empty, Empty), Empty, Empty)" in {
    buildAST("(1)") should equal (Expr(Term(Expr(Term(Factor(1), Empty, Empty), Empty, Empty), Empty, Empty), Empty, Empty))
  }

  "[+ 2], [2 +], [1 + 2;]" should "throw Error" in {
    assertThrows[Error] {
      buildAST("+ 2")
    }
    assertThrows[Error] {
      buildAST("2 +")
    }
    assertThrows[Error] {
      buildAST("1 + 2;")
    }
  }
}
