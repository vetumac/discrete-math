import exceptions._

object App {
  def main(args: Array[String]): Unit = {
    try {
      val sle = SystemOfLinearEquationsMatrix(IOService.getDataFromFile("input.txt"))
      println("Input matrix:")
      println(sle)
      try {
        println("Gauss nethod:")
        Gauss(sle).foreach(f => print(f + " "))
      } catch {
        case ex: InvalidGaussMatrixSizeException => println("System of Linear Equation not resolver by Gauss Method")
      }
      try {
        println("\nHoletsky nethod:")
        Holetsky(sle).foreach(f => print(f + " "))
      } catch {
        case ex: NonSimetricMatrixException => println("Not simetric matrix. Holetsky Method not avalible")
      }
    }
    catch {
      case ex: NumberFormatException => println("Invalid numer format " + ex.getMessage)
      case ex: InvalidMatrixException => println("Invalid matrix dimensions")
      case ex: InvalidSystemOfLinearEquationsMatrixException => println("Invalid System of Linear Equation Matrix")
      case ex: NoUniqueSolutionException => println("No Unique Solution")
    }
  }
}
