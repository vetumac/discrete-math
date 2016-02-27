import exceptions.{InvalidGaussMatrixSizeException, InvalidMatrixException, InvalidSystemOfLinearEquationsMatrixException, NoUniqueSolutionException}

object App {
  def main(args: Array[String]): Unit = {
    try {
      val sle = SystemOfLinearEquationsMatrix(IOService.getDataFromFile("input.txt"))
      println(Gauss(sle))
      //print()
    }
    catch {
      case ex: NumberFormatException => println("Invalid numer format " + ex.getMessage)
      case ex: InvalidMatrixException => println("Invalid matrix dimensions")
      case ex: InvalidSystemOfLinearEquationsMatrixException => println("Invalid System of Linear Equation Matrix")
      case ex: InvalidGaussMatrixSizeException => println("System of Linear Equation not resolver y Gauss Method")
      case ex: NoUniqueSolutionException => println("No Unique Solution")
    }
  }
}
