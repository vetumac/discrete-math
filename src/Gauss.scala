import exceptions.InvalidGaussMatrix

object Gauss extends Function1[SystemOfLinearEquationsMatrix, List[Double]] {

  def apply(sle: SystemOfLinearEquationsMatrix): List[Double] = {
    List()
  }

  def checkGaussMatrix(sle: SystemOfLinearEquationsMatrix): SystemOfLinearEquationsMatrix = {

    sle.matrix.head.head match {
      case 0 => throw new InvalidGaussMatrix()
      case _ => sle.matrix.size match {
        case 1 => sle
        case _ => checkGaussMatrix(SystemOfLinearEquationsMatrix(sle.matrix.tail.map(f => f.tail)))
      }
    }
  }
}
