import exceptions.InvalidMatrixException

abstract class AbstractMatrix(matrix: List[List[Double]]) {

  checkMatrixDimensions(matrix)

  def checkMatrixDimensions(matrix: List[List[Double]]) = {
    try {
      val firstLength = matrix.head.size
      val differentLengthElement = matrix.tail.find(p => p.size != firstLength)
      differentLengthElement.isEmpty match {
        case false => throw new InvalidMatrixException()
        case true => ;
      }
    }
    catch {
      case e: UnsupportedOperationException => throw new InvalidMatrixException()
    }
  }
}
