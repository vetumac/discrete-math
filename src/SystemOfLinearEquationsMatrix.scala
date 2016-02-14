import exceptions.InvalidSystemOfLinearEquationsMatrixException

case class SystemOfLinearEquationsMatrix(val matrix: List[List[Double]]) extends AbstractMatrix(matrix) {

  checkSystemOfLinearEquationsMatrix(matrix)

  def checkSystemOfLinearEquationsMatrix(matrix: List[List[Double]]) = {
    matrix.head.size == matrix.size + 1 match {
      case false => throw new InvalidSystemOfLinearEquationsMatrixException()
      case true => ;
    }
  }

}
