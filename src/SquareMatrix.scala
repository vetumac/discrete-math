import exceptions.InvalidSquareMatrixException

case class SquareMatrix(matrix: List[List[Double]]) extends AbstractMatrix(matrix) {

  checkSquareMatrix(matrix) match {
    case false => throw new InvalidSquareMatrixException()
    case true => ;
  }

  def checkSquareMatrix(matrix: List[List[Double]]) = matrix.head.size == matrix.size

  def determinant(): Double = {
    determinantCalc(matrix: List[List[Double]])
  }

  private def determinantCalc(matrix: List[List[Double]]): Double = {
    matrix.length match {
      case 1 => matrix.head.head
      case _ => matrix.head.foldLeft(0, 0.0)((r: (Int, Double), cur: Double) => (r._1 + 1, r._2 + plusMinusOne(r._1) * cur
        * determinantCalc(matrix.tail.map(ff => List(ff.take(r._1), ff.drop(r._1 + 1)).flatten))))._2
    }
  }

  def plusMinusOne(j: Int): Int = {
    j % 2 match {
      case 0 => 1
      case 1 => -1
    }
  }
}