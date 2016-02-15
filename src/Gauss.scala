import exceptions.InvalidGaussMatrix

object Gauss extends ((SystemOfLinearEquationsMatrix) => List[List[Double]]) {

  def apply(sle: SystemOfLinearEquationsMatrix): List[List[Double]] = {
    checkGaussMatrix(sle)

    val fS = firstStep(sle.matrix)
    println(fS)

    sle.matrix
  }

  def checkGaussMatrix(sle: SystemOfLinearEquationsMatrix): Boolean = {
    sle.matrix.head.head match {
      case 0 => throw new InvalidGaussMatrix()
      case _ => sle.matrix.size match {
        case 1 => true
        case _ => checkGaussMatrix(SystemOfLinearEquationsMatrix(sle.matrix.tail.map(f => f.tail)))
      }
    }
  }

  def firstStep(sle: List[List[Double]]): List[List[Double]] = {
    val dH = sle.head.map(f => f / sle.head.head)
    sle.size match {
      case 1 => dH :: Nil
      case _ =>
        val dM = dH :: sle.tail.map(f => {
          f.indices.toList.map(i => f(i) - f(i) * dH(i))
        })
        val mM = firstStep(dM.tail.map(f => f.tail))
        dH :: mM.map(f => 0.0 :: f)
    }
  }

  def secondStep(sle: List[List[Double]]): List[List[Double]] = {
    List()
  }
}
