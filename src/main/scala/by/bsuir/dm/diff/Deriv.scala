package by.bsuir.dm.diff

import net.sourceforge.jeval.Evaluator

object Deriv {

  def apply(func: String, a: Double, b: Double, x: Double, n: Int): (Double, Double) = {
    a < x && x < b && n > 3 match {
      case false => throw new IllegalArgumentException
      case true =>
        val evaluator = new Evaluator
        val h = (b - a) / n
        val nearlyPoint = ((x + h / 2 - a) / h).toInt
        nearlyPoint match {
          case 0 =>
            val y = (0 to 4).map(i => {
              evaluator.putVariable("x", (i * h).toString)
              evaluator.evaluate(func).toDouble
            })
            val firstDiff = (-3 * y(0) + 4 * y(1) - y(2)) / (2 * h)
            val secondDiff = (2 * y(0) - 5 * y(1) + 4 * y(2) - y(3)) / math.pow(h, 2)
            (firstDiff, secondDiff)
          case `n` =>
            val y = (n - 3 to n).map(i => {
              evaluator.putVariable("x", (i * h).toString)
              evaluator.evaluate(func).toDouble
            })
            val firstDiff = (y(1) - 4 * y(2) + 3 * y(3)) / (2 * h)
            val secondDiff = (-y(0) + 4 * y(1) - 5 * y(2) + 2 * y(3)) / math.pow(h, 2)
            (firstDiff, secondDiff)
          case _ =>
            val y = (n - 1 to n + 1).map(i => {
              evaluator.putVariable("x", (i * h).toString)
              evaluator.evaluate(func).toDouble
            })
            val firstDiff = (-y(0) + y(2)) / (2 * h)
            val secondDiff = (y(0) - 2 * y(1) + y(2)) / math.pow(h, 2)
            (firstDiff, secondDiff)
        }
    }
  }
}
