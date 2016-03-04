import exceptions.InvalidSystemOfLinearEquationsMatrixException

class SystemOfLinearEquationsMatrix(mtrx: List[List[Double]]) extends AbstractMatrix(mtrx) {

  SystemOfLinearEquationsMatrix.checkMatrix(matrix) match {
    case false => throw new InvalidSystemOfLinearEquationsMatrixException()
    case true => ;
  }

  override def toString = {
    mtrx.foldLeft("")((a, c) => a + c.foldLeft("")((aa, cc) => aa + cc + " ") + "\n")
  }
}

object SystemOfLinearEquationsMatrix {

  def apply(matrix: List[List[Double]]) = new SystemOfLinearEquationsMatrix(matrix)

  def checkMatrix(matrix: List[List[Double]]) =
    matrix.head.length == matrix.length + 1 /* && !matrix.find(p => p.length != matrix.head.length).contains()*/
}
