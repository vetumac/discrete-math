package by.bsuir.dm.aprox

object CubicSpline {
  def apply(x: Double, xy: List[(Double, Double)]): Double = {

    def binarySearch(x: Double, list: List[(Double, Double, Double, Double, Double)]): (Double, Double, Double, Double, Double) = x match {
      case (x: Double) if x < list.head._5 => list.head
      case (x: Double) if list(list.length - 2)._5 <= x => list.last
      case (x: Double) if x < list(list.length / 2)._5 => binarySearch(x, list.dropRight(list.length / 2))
      case (x: Double) if list(list.length / 2)._5 <= x => binarySearch(x, list.drop(1 + list.length / 2))
    }

    val splines = xy.sliding(3).scanLeft((xy.head, (0.0, 0.0)))((acc, cur) => {
      val a = cur(1)._1 - cur.head._1
      val b = cur(2)._1 - cur(1)._1
      val c = 2 * (a + b)
      val f = 6 * ((cur(2)._2 - cur(1)._2) / b - (cur(1)._2 - cur.head._2))
      val z = a * acc._2._1 + c
      val alpha = -b / z
      val beta = (f - a * acc._2._2) / z
      (cur(1), (alpha, beta))
    }).scanRight((xy.last, 0.0))((cur, acc) => {
      val c = cur._2._1 * acc._2 + cur._2._2
      (cur._1, c)
    }).sliding(2).scanLeft((xy.head._2, 0.0, 0.0, 0.0, xy.head._1))((acc, cur) => {
      val hi = cur(1)._1._1 - cur.head._1._1
      val d = (cur(1)._2 - cur.head._2) / hi
      val b = hi * (2 * cur(1)._2 + cur.head._2) / 6 + (cur(1)._1._2 - cur.head._1._2) / hi
      (cur(1)._1._2, b, cur(1)._2, d, cur(1)._1._1)
    }).toList

    val spline = binarySearch(x, splines)
    val dx = x - spline._5
    spline._1 + (spline._2 + (spline._3 / 2 + spline._4 * dx / 6) * dx) * dx
  }
}
