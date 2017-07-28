import calculator.{Evaluator, Parser, Tokenizer}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by dawning on 2017/7/28.
  */
class CalculatorTest extends FlatSpec {
  def calc = Tokenizer.read _ andThen Parser.parse andThen Evaluator.eval

  "1 + 2 + 3" should "equal 6" in {
    calc("1 + 2 + 3") should be (6.0)
  }

  "2 * 2 + 3" should "equal 6" in {
    calc("2 * 2 + 3") should be (7.0)
  }

  "1 + 2 * 3" should "equal 6" in {
    calc("1 + 2 * 3") should be (7.0)
  }

  "2 / 0" should "equal 6" in {
    calc("2 / 0") should be (Double.PositiveInfinity)
  }
}
