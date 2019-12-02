package solution1

// TODO: find out how to unwrap these with cats probably.
//case class ModuleMass(moduleMass: Int)
//case class FuelRequirement(fuelRequirement: Int)

object Solution {
  private def calculateFuelRequirement(moduleMass: Int): Int = {
    Math.floor(moduleMass / 3).toInt - 2
  }

  private def calculateSumFuelRequirements(moduleMasses: Seq[Int], fuelRequirementCalcFunction: Int => Int): Int = {
    moduleMasses.map(fuelRequirementCalcFunction).sum
  }

  /* Part1: Fuel requirements. */
  def calculateSumFuelRequirementsPart1(moduleMasses: Seq[Int]): Int = {
    calculateSumFuelRequirements(moduleMasses, calculateFuelRequirement)
  }

  private def calculateRealFuelRequirement(moduleMass: Int): Int = {
    @scala.annotation.tailrec
    def loop(currFuelRequirement: Int, accFuelRequirement: Int): Int = {
      if (currFuelRequirement <= 0) {
        accFuelRequirement
      } else {
        loop(calculateFuelRequirement(currFuelRequirement), accFuelRequirement+currFuelRequirement)
      }
    }

    loop(calculateFuelRequirement(moduleMass), 0)
  }

  /* Part2: Fuel requirements considering fuel weight of density 1. */
  def calculateSumFuelRequirementsPart2(moduleMasses: Seq[Int]): Int = {
    calculateSumFuelRequirements(moduleMasses, calculateRealFuelRequirement)
  }
}
