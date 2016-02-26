import exceptions.InvalidSystemOfLinearEquationsMatrixException

case class SystemOfLinearEquationsMatrix(matrix: List[List[Double]]) extends AbstractMatrix(matrix) {

  checkSystemOfLinearEquationsMatrix(matrix) match {
    case false => throw new InvalidSystemOfLinearEquationsMatrixException()
    case true => ;
  }

  def checkSystemOfLinearEquationsMatrix(matrix: List[List[Double]]) =
    matrix.head.length == matrix.length + 1 && !matrix.find(p => p.length != matrix.head.length).contains()
}
