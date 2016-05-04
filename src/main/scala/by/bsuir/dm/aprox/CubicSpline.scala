package by.bsuir.dm.aprox

object CubicSpline {
  def apply(x: Double, points: List[(Double, Double)]): Double = {
    points.sliding(3).scanLeft((points.head, (0.0, 0.0)))((acc, cur) => {
      val a = cur(1)._1 - cur.head._1
      val b = cur(2)._1 - cur(1)._1
      val c = 2 * (a + b)
      val f = 6 * ((cur(2)._2 - cur(1)._2) / b - (cur(1)._2 - cur.head._2))
      val z = a * acc._2._1 + c
      val alpha = -b / z
      val beta = (f - a * acc._2._2) / z
      ((cur.head._1, cur.head._2), (alpha, beta))
    }).scanRight((points.last, 0.0))((cur, acc) => {
      val c = cur._2._1 * acc._2 + cur._2._2
      ((cur._1._1, cur._1._2), c)
    }).sliding(2).map(tuple => {
      val hi = tuple(1)._1._1 - tuple.head._1._1
      val d = (tuple(1)._2 - tuple.head._2) / hi
      val b = hi * (2 * tuple(1)._2 + tuple.head._2) / 6 + (tuple(1)._1._2 - tuple.head._1._2) / hi
      ()
    })
    2.0
  }
}
