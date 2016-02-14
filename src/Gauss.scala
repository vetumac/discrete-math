object Gauss extends Function1[SystemOfLinearEquationsMatrix, List[Double]] {

  def apply(matrix: SystemOfLinearEquationsMatrix): List[Double] = {
    List()
  }

  def checkGaussMatrix(matrix: SystemOfLinearEquationsMatrix): Boolean = {
    true
  }

}
