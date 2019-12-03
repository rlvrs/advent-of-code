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

  def parseLines(resourcePath: String): Seq[String] = {
    val bufferedSource = Source.fromResource(resourcePath)
    val fileLines = bufferedSource.getLines.toList
    bufferedSource.close

    fileLines
  }

  private def readFileAsString(resourcePath: String): String = {
    parseLines(resourcePath).mkString
  }

  def parseLinesAsSignedIntList(resourcePath: String): Seq[Int] = {
    parseLines(resourcePath).flatMap(parseSignedNumber)
  }

  def parseLinesAsIntList(resourcePath: String): Seq[Int] = {
    parseLines(resourcePath).flatMap(parseNumber)
  }
}
