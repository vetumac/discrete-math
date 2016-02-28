object Holetsky {

  def apply(sle: SystemOfLinearEquationsMatrix): List[Double] = {
    List(0)
  }

  private def firstStep(a: SquareMatrix, l: List[List[Double]]): SquareMatrix = {
    val extL = extendMatrix(l)


    //l.matrix.head.head
    SquareMatrix(List(List(0)))
  }

  private def extendMatrix(matrix: List[List[Double]]): List[List[Double]] = {
    matrix.map(f => f :+ 0.0) :+ List()
  }
}
