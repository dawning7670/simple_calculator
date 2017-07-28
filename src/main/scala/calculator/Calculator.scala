package calculator

/**
  * Created by dawning on 2017/7/25.
  */
object Calculator {
  def main(args: Array[String]): Unit = {
    val expr = "1 + 2 + 3"
    val tokens = Tokenizer.read(expr)
    val ast = Parser.parse(tokens)
    val value = Evaluator.eval(ast)
    println(value)
  }
}
