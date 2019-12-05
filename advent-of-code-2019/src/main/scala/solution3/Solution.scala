package solution3

import parser.Parser.parseNumber

// TODO: companion object for Coordinates
// TODO: case class for directions
// TODO: revisit generateAllCoordinates

final case class Coordinate(x: Int, y: Int) {
  def +(that: Coordinate): Coordinate = Coordinate(this.x + that.x, this.y + that.y)
  def *(that: Coordinate): Coordinate = Coordinate(this.x * that.x, this.y * that.y)
}

object Solution {
  private def getPosition(wire: List[Coordinate], coordinate: Coordinate): Int = {
    wire.indexOf(coordinate) + 1
  }

  private def minimizeIntersections(wire1: List[Coordinate], wire2: List[Coordinate], mapToInt: Coordinate => Int): Int = {
    wire1.intersect(wire2)
      .map(mapToInt)
      .min
  }

  def minSignalDelayIntersection(wire1: List[Coordinate], wire2: List[Coordinate]): Int = {
    val signalDelay = (coordinate: Coordinate) => getPosition(wire1,coordinate) + getPosition(wire2,coordinate)
    minimizeIntersections(wire1, wire2, signalDelay)
  }

  def closestManhattanIntersection(wire1: List[Coordinate], wire2: List[Coordinate]): Int = {
    val manhattanDistance = (coordinate: Coordinate) => Math.abs(coordinate.x) + Math.abs(coordinate.y)
    minimizeIntersections(wire1, wire2, manhattanDistance)
  }

  private def generateRelativeCoordinates(instruction: String): List[Coordinate] = {
    (instruction match {
      case s"R${stepsNumber}" => nextXCoordinates(parseNumber(stepsNumber).getOrElse(0))
      case s"U${stepsNumber}" => nextYCoordinates(parseNumber(stepsNumber).getOrElse(0))
      case s"D${stepsNumber}" => nextYCoordinates(parseNumber(stepsNumber).getOrElse(0)).map(negateY)
      case s"L${stepsNumber}" => nextXCoordinates(parseNumber(stepsNumber).getOrElse(0)).map(negateX)
      case _ => List().toIndexedSeq
    }).toList
  }

  private def adjustToNewOrigin(origin: Coordinate, coordinates: List[Coordinate]): List[Coordinate] = {
    coordinates.map(_ + origin)
  }

  def generateCoordinates(origin: Coordinate, instruction: String): List[Coordinate] = {
    adjustToNewOrigin(origin, generateRelativeCoordinates(instruction))
  }

  private def negateY(coordinate: Coordinate): Coordinate = coordinate * Coordinate(1,-1)

  private def negateX(coordinate: Coordinate): Coordinate = coordinate * Coordinate(-1,1)

  private def nextXCoordinates(n: Int) = {
    for (i <- 1 to n) yield Coordinate(i, 0)
  }

  private def nextYCoordinates(n: Int) = {
    for (i <- 1 to n) yield Coordinate(0, i)
  }

  def generateAllCoordinates(instructions: List[String]): List[Coordinate] = {
    val allRelativeCoordinates = instructions.map(instruction => {
      generateCoordinates(Coordinate(0,0), instruction)
    })

    val allOrigins = allRelativeCoordinates
      .map(_.last)
      .dropRight(1)
      .scanLeft(Coordinate(0,0))(_+_)

    allOrigins.zip(allRelativeCoordinates)
      .flatMap {
        case (origin:Coordinate, coordinates: List[Coordinate]) => adjustToNewOrigin(origin, coordinates)
        case _ => List()
      }
  }
}
