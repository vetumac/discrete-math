import exceptions.NoUniqueSolutionException

object Gauss extends ((SystemOfLinearEquationsMatrix) => List[Double]) {

  def apply(sle: SystemOfLinearEquationsMatrix): List[Double] = {
    SquareMatrix(sle.matrix.map(f => f.dropRight(1))).determinant match {
      case 0 => throw new NoUniqueSolutionException()
      case _ =>
        val fs = firstStep(sle.matrix)
        secondStep(fs)
    }
  }

  def firstStep(sle: List[List[Double]]): List[List[Double]] = {
    val dH = sle.head.map(f => f / sle.head.head)
    sle.size match {
      case 1 => dH :: Nil
      case _ =>
        val dM = dH :: sle.tail.map(f => {
          f.indices.toList.map(i => f(i) - f.head * dH(i))
        })
        val mM = firstStep(dM.tail.map(f => f.tail))
        dH :: mM.map(f => 0.0 :: f)
    }
  }

  def secondStep(sle: List[List[Double]]): List[Double] = {
    sle.size match {
      case 1 => sle.head.tail
      case _ =>
        val solvs = secondStep(sle.tail.map(f => f.tail))
        val koefs = sle.head.tail.reverse.tail
        val solv = sle.head.last - koefs.indices.map(f => koefs(f) * solvs(f)).sum
        solv :: solvs
    }
  }
}
