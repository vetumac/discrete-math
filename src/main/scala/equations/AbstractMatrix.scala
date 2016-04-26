package equations

import exceptions.InvalidMatrixException

abstract class AbstractMatrix(mtrx: List[List[Double]]) {

  val matrix = mtrx

  checkMatrix match {
    case true => ;
    case false => throw new InvalidMatrixException()
  }

  def checkMatrix = {
    try {
      val firstLength = matrix.head.size
      val differentLengthElement = matrix.tail.find(p => p.size != firstLength)
      differentLengthElement.isEmpty
    }
    catch {
      case e: UnsupportedOperationException => throw new InvalidMatrixException()
    }
  }
}
