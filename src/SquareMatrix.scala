import exceptions.InvalidSquareMatrixException

class SquareMatrix(matrix: List[List[Double]]) extends AbstractMatrix(matrix) {

  matrix.head.size == matrix.size match {
    case false => throw new InvalidSquareMatrixException()
    case true => ;
  }

  def determinant: Double = {
    SquareMatrix.determinant(matrix)
  }

  def isSimetric: Boolean = {
    SquareMatrix.isSimetric(matrix)
  }
}

object SquareMatrix {

  def apply(matrix: List[List[Double]]) = new SquareMatrix(matrix)

  def determinant(matrix: List[List[Double]]): Double = {
    matrix.length match {
      case 1 => matrix.head.head
      case _ => matrix.head.foldLeft(0, 0.0)((r: (Int, Double), cur: Double) => (r._1 + 1, r._2 + plusMinusOne(r._1) * cur
        * determinant(matrix.tail.map(ff => List(ff.take(r._1), ff.drop(r._1 + 1)).flatten))))._2
    }
  }

  private def plusMinusOne(j: Int): Int = {
    j % 2 match {
      case 0 => 1
      case 1 => -1
    }
  }

  def isSimetric(matrix: List[List[Double]]): Boolean = {
    matrix.size match {
      case 1 => true
      case _ =>
        val row = matrix.head.tail
        val col = matrix.tail.map(f => f.head)
        row.zip(col).exists(p => p._1 != p._2) match {
          case true => false
          case false => isSimetric(matrix.tail.map(f => f.tail))
        }
    }
  }
}

