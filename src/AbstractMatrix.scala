import exceptions.InvalidMatrixException

abstract class AbstractMatrix(matrix: List[List[Double]]) {

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
