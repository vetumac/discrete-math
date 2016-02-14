import exceptions.{InvalidMatrixException, InvalidSystemOfLinearEquationsMatrixException}

object App {
  def main(args: Array[String]): Unit = {
    try {
      println(SystemOfLinearEquationsMatrix(IOService.getDataFromFile("input.txt")))
    }
    catch {
      case ex: NumberFormatException => println("Invalid numer format " + ex.getMessage)
      case ex: InvalidMatrixException => println("Invalid matrix dimensions")
      case ex: InvalidSystemOfLinearEquationsMatrixException => println("Invalid System of Linear Equation Matrix")
    }
  }
}
