package by.bsuir.dm

import scala.io.Source

object IOService {

  def getDoubleDataFromFile(name: String): List[List[Double]] = Source.fromFile(name).getLines().map(line =>
    line.split(" ").map(num => num.toDouble).toList
  ).toList

  def getStringDataFromFile(name: String): List[List[String]] = Source.fromFile(name).getLines().map(line =>
    line.split(" ").toList
  ).toList

}
