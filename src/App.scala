import exceptions.InvalidMatrixException

object App {
  def main(args: Array[String]): Unit = {
    try {
      println(IOService.readMatrixFromFile("input.txt"))
    }
    catch {
      case ex: NumberFormatException => println("Invalid numer format " + ex.getMessage)
      case ex: InvalidMatrixException => println("Invalid matrix dimensions")
    }
  }
}
