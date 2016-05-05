package by.bsuir.dm

import java.io.FileNotFoundException

import by.bsuir.dm.aprox.CubicSpline
import by.bsuir.dm.diff.{Deriv, Integr}
import by.bsuir.dm.equations.{Gauss, Holetsky, SystemOfLinearEquationsMatrix}
import by.bsuir.dm.exceptions._

import scalax.chart.module.Charting._

object App {
  def main(args: Array[String]): Unit = {
    args(0) match {
      case "-e" => equation()
      case "-d" => diff()
      case "-a" => aprox()
    }
  }

  private def equation() = {
    try {
      val sle = SystemOfLinearEquationsMatrix(IOService.getDoubleDataFromFile("equation.txt"))
      println("Input matrix:")
      println(sle)
      try {
        println("Gauss method:")
        Gauss(sle).foreach(f => print(f + " "))
      } catch {
        case ex: InvalidGaussMatrixSizeException => println("System of Linear Equation not resolver by Gauss Method")
      }
      try {
        println("\nHoletsky method:")
        Holetsky(sle).foreach(f => print(f + " "))
      } catch {
        case ex: NonSimetricMatrixException => println("Not symmetric matrix. Holetsky Method not available")
        case ex: FakeMatrixForHoletskiMetjod => println("Fake matrix for Holetsky Method")
      }
    }
    catch {
      case ex: NumberFormatException => println("Invalid number format " + ex.getMessage)
      case ex: InvalidMatrixException => println("Invalid matrix dimensions")
      case ex: InvalidSystemOfLinearEquationsMatrixException => println("Invalid System of Linear Equation Matrix")
      case ex: NoUniqueSolutionException => println("No Unique Solution")
      case ex: FileNotFoundException => println("File equation.txt not found")
    }
  }

  private def diff() = {
    try {
      val inputData = IOService.getStringDataFromFile("diff.txt")
      println("Input data:")
      inputData.head.foreach(str => print(str + " "))
      println()
      val derivativeFunc = inputData.head.head
      val a = inputData.head(2).toDouble
      val b = inputData.head(3).toDouble
      val n = inputData.head(4).toInt
      println("First and second derivatives: " + Deriv(derivativeFunc, a, b, (a + b) / 2, n))
      val integralFunc = inputData.head(1)
      val d = inputData.head(5).toDouble
      println("Integral: " + Integr(integralFunc, a, b, d))
    }
    catch {
      case ex: FileNotFoundException => println("File diff.txt not found")
      case ex: IllegalArgumentException => println("Illegal input data")
      case ex: NumberFormatException => println("Invalid number format " + ex.getMessage)
    }
  }

  private def aprox() = {
    try {
      val points = IOService.getDoubleDataFromFile("aprox.txt").map(row => (row.head, row(1)))
      //val resultLagrange = (185 to 1900).map(number => (number.toDouble / 1000, Lagrange(number.toDouble / 1000, points)))
      val resultCubicSpline = (-10 to 220).map(number => (number.toDouble / 100, CubicSpline(number.toDouble / 100, points)))
      val data = List(("Input", points), /*("Aproxitated Lagrange", resultLagrange),*/ ("Aproxitated Cubic Splain", resultCubicSpline))
      val chart = XYLineChart.shapes(data, "Aproximate")
      chart.saveAsPNG("aproximate.png", (800, 600))
      println("Aproximate results in aproximate.png")
    } catch {
      case ex: FileNotFoundException => println("File aprox.txt not found")
      case ex: IndexOutOfBoundsException => println("Illegal input data")
      case ex: NumberFormatException => println("Invalid number format " + ex.getMessage)
    }
  }
}
