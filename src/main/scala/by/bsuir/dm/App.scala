package by.bsuir.dm

import java.io.FileNotFoundException

import by.bsuir.dm.aprox.{CubicSpline, Lagrange}
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

    def scale(pixel: Int, maxPixel: Int, abroad: Double, firstX: Double, lastX: Double): Double =
      firstX - abroad * (lastX - firstX) + (1 + 2 * abroad) * (lastX - firstX) * pixel / maxPixel

    try {
      val points = IOService.getDoubleDataFromFile("aprox.txt").map(row => (row.head, row(1)))
      val plotSize = 1000

      val scaledInBoard = scale(_: Int, plotSize, -0.1, points.head._1, points.last._1)
      val scaledOutBoard = scale(_: Int, plotSize, 0.1, points.head._1, points.last._1)

      val lagrange = Lagrange(_: Double, points)
      val resultLagrange = (0 to plotSize).map(number => (scaledInBoard(number), lagrange(scaledInBoard(number))))
      val dataLagrange = List(("Input", points), ("Aproxitated Lagrange", resultLagrange))
      val chartLagrange = XYLineChart.shapes(dataLagrange, "Aproximate Lagrange")
      chartLagrange saveAsPNG("lagrange.png", (plotSize, plotSize * 3 / 4))
      println("Aproximate Lagrange results in lagrange.png")

      val cubicSpline = CubicSpline(_: Double, points)
      val resultCubicSpline = (0 to plotSize).map(number => (scaledOutBoard(number), cubicSpline(scaledOutBoard(number))))
      val dataCuicSpline = List(("Input", points), ("Aproxitated Cubic Splain", resultCubicSpline))
      val chartCubicSpline = XYLineChart.shapes(dataCuicSpline, "Aproximate Cubic Splain")
      chartCubicSpline saveAsPNG("cubic-spline.png", (plotSize, plotSize * 3 / 4))
      println("Aproximate Cubic Splain results in cubic-spline.png")

    } catch {
      case ex: FileNotFoundException => println("File aprox.txt not found")
      case ex: IndexOutOfBoundsException => println("Illegal input data")
      case ex: NumberFormatException => println("Invalid number format " + ex.getMessage)
    }
  }
}
