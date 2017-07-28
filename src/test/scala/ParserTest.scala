import calculator._
import org.scalatest.FlatSpec

/**
  * Created by dawning on 2017/7/25.
  */
class ParserTest extends FlatSpec {

  import calculator.Parser._

  def buildAST(in: String): AST = {
    parse(Tokenizer.read(in))
  }

  //  "[1 + 2], [1 + 2 - 3], [1 * 2 + 3], [2 + 3 * 4]" should "match expr" in {
  //    tokens = Tokenizer.read("1 + 2")
  //    assert(expr())
  //    tokens = Tokenizer.read("1 + 2 - 3")
  //    assert(expr())
  //    tokens = Tokenizer.read("1 * 2 + 3")
  //    assert(expr())
  //    tokens = Tokenizer.read("2 + 3 * 4")
  //    assert(expr())
  //  }
  //
  //  "[+ 2], [2 +], [1 + 2;]" should "throw Error" in {
  //    assertThrows[Error] {
  //      tokens = Tokenizer.read("+ 2")
  //      expr()
  //    }
  //    assertThrows[Error] {
  //      tokens = Tokenizer.read("2 +")
  //      expr()
  //    }
  //    assertThrows[Error] {
  //      tokens = Tokenizer.read("1 + 2;")
  //      expr()
  //    }
  //  }

  //  "1 + 2" should "build AST Plus(1.0, 2.0)" in {
  //    buildAST("1 + 2") should equal(PlusNode(LiteralNode(1.0), LiteralNode(2.0)))
  //  }
  //
  //  "1 + 2 * 3" should "build AST Plus(1.0, Multi(2.0, 3.0))" in {
  //    buildAST("1 + 2 * 3") should equal(PlusNode(LiteralNode(1.0), MultiNode(LiteralNode(2.0), LiteralNode(3.0))))
  //  }
  //
  //  "1 * 2 + 3" should "build AST Plus(Multi(1.0, 2.0), 3.0)" in {
  //    buildAST("1 * 2 + 3") should equal(PlusNode(MultiNode(LiteralNode(1.0), LiteralNode(2.0)), LiteralNode(3.0)))
  //  }
}
