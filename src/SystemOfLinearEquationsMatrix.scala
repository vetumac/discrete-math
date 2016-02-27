import exceptions.InvalidSystemOfLinearEquationsMatrixException

class SystemOfLinearEquationsMatrix(mtrx: List[List[Double]]) extends AbstractMatrix(mtrx) {

  SystemOfLinearEquationsMatrix.checkMatrix(matrix) match {
    case false => throw new InvalidSystemOfLinearEquationsMatrixException()
    case true => ;
  }
}

object SystemOfLinearEquationsMatrix {

  def apply(matrix: List[List[Double]]) = new SystemOfLinearEquationsMatrix(matrix)

  def checkMatrix(matrix: List[List[Double]]) =
    matrix.head.length == matrix.length + 1 /* && !matrix.find(p => p.length != matrix.head.length).contains()*/
}
