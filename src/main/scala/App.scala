import java.io.FileNotFoundException

import diff.Diff
import equations.{Gauss, Holetsky, SystemOfLinearEquationsMatrix}
import exceptions._

object App {
  def main(args: Array[String]): Unit = {
    args(0) match {
      case "-e" => equation()
      case "-d" => diff()
    }
  }

  def equation() = {
    try {
      val sle = SystemOfLinearEquationsMatrix(IOService.getDoubleDataFromFile("equation.txt"))
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
        case ex: FakeMatrixForHoletskiMetjod => println("Fake matrix for Holetsky Method")
      }
    }
    catch {
      case ex: NumberFormatException => println("Invalid numer format " + ex.getMessage)
      case ex: InvalidMatrixException => println("Invalid matrix dimensions")
      case ex: InvalidSystemOfLinearEquationsMatrixException => println("Invalid System of Linear Equation Matrix")
      case ex: NoUniqueSolutionException => println("No Unique Solution")
      case ex: FileNotFoundException => println("File equation.txt not found")
    }
  }

  def diff() = {
    try {
      val inputData = IOService.getStringDataFromFile("diff.txt")
      println("Input data:")
      inputData.head.foreach(str => print(str + " "))
      println()
      val func = inputData.head.head
      val a = inputData.head(2).toDouble
      val b = inputData.head(3).toDouble
      val n = inputData.head(4).toInt
      println("First and second derivatives: " + Diff(func, a, b, (a + b) / 2, n))
    }
    catch {
      case ex: FileNotFoundException => println("File diff.txt not found")
      case ex: IllegalArgumentException => println("Illegal input data")
    }
  }

}
