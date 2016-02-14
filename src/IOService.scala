import scala.io.Source

object IOService {
  def getDataFromFile(name: String): List[List[Double]] = Source.fromFile(name).getLines().map(line =>
    line.split(" ").map(num => num.toDouble).toList
  ).toList
}



