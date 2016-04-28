package by.bsuir.dm.diff

import net.sourceforge.jeval.Evaluator

object Integr {

  def apply(func: String, a: Double, b: Double, d: Double): Double = {
    a < b match {
      case false => throw new IllegalArgumentException
      case true =>
        val evaluator = new Evaluator
        val firstRes = ((b - a) / 6) * (evaluate(func, a, evaluator) + 4 * evaluate(func, (a + b) / 2, evaluator) + evaluate(func, b, evaluator))
        simpson(func, a, b, d, 2, firstRes, evaluator)
    }
  }

  private def simpson(func: String, a: Double, b: Double, d: Double, n: Int, prevRes: Double, evaluator: Evaluator): Double = {
    val h = (b - a) / (2 * n)
    val nodes = (0 to 2 * n).map(i => a + h * i)
    val res = ((b - a) / (6 * n)) * (evaluate(func, nodes(0), evaluator) + evaluate(func, nodes(2 * n), evaluator)
      + 2 * (1 until n).map(i => evaluate(func, nodes(2 * i), evaluator)).sum
      + 4 * (0 until n).map(i => evaluate(func, nodes(2 * i + 1), evaluator)).sum)
    math.abs(res - prevRes) <= d match {
      case true => res
      case false => simpson(func, a, b, d, 2 * n, res, evaluator)
    }
  }

  private def evaluate(func: String, x: Double, evaluator: Evaluator): Double = {
    evaluator.putVariable("x", x.toString)
    evaluator.evaluate(func).toDouble
  }
}
