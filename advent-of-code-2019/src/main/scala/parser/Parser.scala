package parser

import scala.io.Source

object Parser {
  def parseCommaSeparatedIntList(resourcePath: String): List[Int] = {
    parseCommaSeparatedTextFile(resourcePath)
      .flatMap(parseNumber)
  }

  private def parseCommaSeparatedTextFile(resourcePath: String): List[String] = {
    readFileAsString(resourcePath).split(",").toList
  }

  private def readFileAsString(resourcePath: String): String = {
    parseLines(resourcePath).mkString
  }

  def parseNumber(numberString: String): Option[Int] = {
    try {
      Some(numberString.toInt)
    } catch {
      case _: Exception => None
    }
  }

  def parseSignedNumber(signedNumberString: String): Option[Int] = {
    signedNumberString.trim match {
      case s"-$numberString" => parseNumber(numberString).map(_ * -1)
      case s"+$numberString" => parseNumber(numberString)
      case "0" => Some(0)
      case _ => None
    }
  }

  def parseLinesAsCommaSeparatedStringList(resourcePath: String): List[List[String]] = {
    parseLines(resourcePath)
      .map(_.split(",").toList)
      .toList
  }

  def parseLinesAsAdjacencyList(resourcePath: String): Map[String, List[String]] = {
    parseLines(resourcePath)
      .map(_.split("\\)"))
      .groupBy(_.head)
      .view.mapValues(_.flatten.toList)
      .toMap
      .map{ case (k,v) => (k, v.filterNot {_.equals(k)}) }
  }

  private def parseLinesAsAdjacencyListReverse(resourcePath: String): Map[String, List[String]] = {
    parseLines(resourcePath)
      .map(_.split("\\)").reverse)
      .groupBy(_.head)
      .view.mapValues(_.flatten.toList)
      .toMap
      .map{ case (k,v) => (k, v.filterNot {_.equals(k)}) }
  }

  private def mergeAdjacencyLists(adjacencyList1: Map[String, List[String]], adjacencyList2: Map[String, List[String]]): Map[String, List[String]] = {
    (adjacencyList1.toList ++ adjacencyList2.toList)
      .groupBy(_._1)
      .view.mapValues(_.flatMap(_._2))
      .toMap
  }

  def parseLinesAsAdjacencyListUndirected(resourcePath: String): Map[String, List[String]] = {
    mergeAdjacencyLists(parseLinesAsAdjacencyListReverse(resourcePath),
      parseLinesAsAdjacencyList(resourcePath))
  }

  def parseLines(resourcePath: String): Seq[String] = {
    val bufferedSource = Source.fromResource(resourcePath)
    val fileLines = bufferedSource.getLines.toList
    bufferedSource.close

    fileLines
  }

  def parseLinesAsSignedIntList(resourcePath: String): Seq[Int] = {
    parseLines(resourcePath).flatMap(parseSignedNumber)
  }

  def parseLinesAsIntList(resourcePath: String): Seq[Int] = {
    parseLines(resourcePath).flatMap(parseNumber)
  }
}
