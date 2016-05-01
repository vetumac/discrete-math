package by.bsuir.dm.aprox

object Lagrange {
  def apply(x: Double, points: List[(Double, Double)]): Double = {
    points.map(point => {
      point._2 * points.foldLeft(1.0)((acc, these) =>
      point._1 == these._1 match {
        case true => acc
        case false => acc * (x - these._1) / (point._1 - these._1)
      })
    }).sum
  }
}
