import exceptions.{FakeMatrixForHoletskiMetjod, NonSimetricMatrixException}

object Holetsky {

  def apply(sle: SystemOfLinearEquationsMatrix): List[Double] = {
    val sq = SquareMatrix(sle.matrix.map(f => f.dropRight(1)))
    sq.isSimetric match {
      case true =>
        val b = sle.matrix.map(f => f.last)
        val l = firstStep(sq, Nil)
        val y = secondStep(List.range(0, l.length).map(f => l(f) :+ b(f)).reverse).reverse
        val transpL = SquareMatrix.transpon(l)
        secondStep(List.range(0, transpL.length).map(f => transpL(f) :+ y(f)))
      case false => throw new NonSimetricMatrixException()
    }
  }

  private def firstStep(a: SquareMatrix, l: List[List[Double]]): List[List[Double]] = {
    l.length < a.matrix.length match {
      case true => firstStep(a, fillDiogonal(a, fillNotDiogonal(a, addZeros(l))))
      case false => l
    }
  }

  private def addZeros(matrix: List[List[Double]]): List[List[Double]] =
    matrix.map(f => f :+ 0.0) :+ List[Double]()

  private def fillNotDiogonal(a: SquareMatrix, l: List[List[Double]]): List[List[Double]] = {
    val i = l.length - 1
    val j = l.last.length
    i > j match {
      case true =>
        val newL = l.dropRight(1) :+ (l.last :+ ((1 / l(j)(j)) * (a.matrix(i)(j) - List.range(0, j).foldLeft(List[Double]())((a, k) => a :+ l(i)(k) * l(j)(k)).sum)))
        fillNotDiogonal(a, newL)
      case false => l
    }
  }

  private def fillDiogonal(a: SquareMatrix, l: List[List[Double]]): List[List[Double]] = {
    val i = l.length - 1
    l.dropRight(1) :+ (l.last :+ sqrt(a.matrix(i)(i) - List.range(0, i).foldLeft(List[Double]())((a, k) => a :+ l(i)(k) * l(i)(k)).sum))
  }

  private def sqrt(double: Double): Double = {
    double < 0 match {
      case true => throw new FakeMatrixForHoletskiMetjod()
      case false => Math.sqrt(double)
    }
  }

  private def secondStep(sle: List[List[Double]]): List[Double] = {
    sle.size match {
      case 1 => sle.head.tail
      case _ =>
        val solvs = secondStep(sle.tail.map(f => f.tail))
        val koefs = sle.head.tail.dropRight(1)
        val solv = sle.head.last - koefs.indices.map(f => koefs(f) * solvs(f)).sum
        solv :: solvs
    }
  }
}
