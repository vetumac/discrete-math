import exceptions.InvalidMatrixException

import scala.io.Source

object IOService {
  def readMatrixFromFile(name: String): List[List[Double]] = checkMatrixDimensions(Source.fromFile(name).getLines().map(line =>
    line.split(" ").map(num => num.toDouble).toList
  ).toList)

  def checkMatrixDimensions(matrix: List[List[Double]]): List[List[Double]] = {
    try {
      val minLength = matrix.map(f => f.size).min
      val maxLength = matrix.map(f => f.size).max
      maxLength == minLength match {
        case true => matrix
        case false => throw new InvalidMatrixException()
      }
    }
    catch {
      case e: UnsupportedOperationException => throw new InvalidMatrixException()
    }

  }

}



