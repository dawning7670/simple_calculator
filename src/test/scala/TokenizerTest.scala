import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by dawning on 2017/7/25.
  */
class TokenizerTest extends FlatSpec {

  import calculator.Tokenizer._

  "1 + 2" should "have 3 tokens [Literal(1.0), Plus, Literal(2.0)]" in {
    read("1 + 2") should equal(List(Literal(1.0), Plus, Literal(2.0)))
    read("   1   +    2") should equal(List(Literal(1.0), Plus, Literal(2.0)))
    println(read("1 + 2 + 3 * 4 / 5;"))
  }

  "1 + 2;" should "throw UnknowError" in {
    assertThrows[UnknownError] {
      read("1 + 2;")
    }
  }
}
