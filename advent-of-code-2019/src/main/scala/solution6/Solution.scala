package solution6

import scala.annotation.tailrec

object Solution {
  def calculateNumberOfOrbitsPart2(adjacencyList: Map[String, List[String]], source: String, destiny: String): Int = {
    @tailrec
    def loop(currentNodes: List[String], visited: Set[String], depth: Int): Int = {
      if (currentNodes.contains(destiny)) {
        depth - 2
      } else {
        val nextNodes = currentNodes
          .flatMap(currentNode => adjacencyList.getOrElse(currentNode, List()))
          .filter(currentNode => !visited.contains(currentNode))
        loop(nextNodes, visited ++ currentNodes.toSet, depth + 1)
      }
    }

    loop(adjacencyList(source), Set(source), 1)
  }

  def calculateNumberOfOrbitsPart1(adjacencyList: Map[String, List[String]]): Int = {
    @tailrec
    def loopList(nextNodes: List[String], depth: Int, numberOrbits: Int): Int = {
      nextNodes match {
        case Nil => numberOrbits
        case _ :: tail => loopList(tail, depth, numberOrbits+depth)
      }
    }

    @tailrec
    def loop(currentNodes: List[String], depth: Int, numberOrbits: Int): Int = {
      currentNodes match {
        case Nil => numberOrbits
        case _ :: _ =>
          val nextNodes = currentNodes
            .flatMap(currentNode => adjacencyList.getOrElse(currentNode, List()))
          loop(nextNodes, depth+1, loopList(currentNodes, depth, numberOrbits))
      }
    }

    loop(adjacencyList("COM"), 1, 0)
  }
}
