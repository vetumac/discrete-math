object Holetsky {

  def apply(sle: SystemOfLinearEquationsMatrix): List[Double] = {
    List(0)
  }

  private def firstStep(a: SquareMatrix, l: List[List[Double]]): SquareMatrix = {
    val extL = extendMatrix(l)
    //extL.last.map()
    (0 -> (extL.length - 1))


    //l.matrix.head.head
    SquareMatrix(List(List(0)))
  }

  private def extendMatrix(matrix: List[List[Double]]): List[List[Double]] = {
    matrix.map(f => f :+ 0.0) :+ List()
  }

  private def addNotDiogonal(a: SquareMatrix, l: List[List[Double]], lLast: List[Double]): List[List[Double]] = {
    val lAdd = l :+ lLast
    val i = lAdd.length - 1
    val j = lAdd.length - 1
    j > i match {
      case true => addNotDiogonal(a, l, lLast :+ ((1 / lAdd(i)(i)) * (a.matrix(i)(j) - List.range(0, j - 1).map(k => l(i)(k) * l(j)(k)).sum)))
      case false => lAdd
    }
  }
}
