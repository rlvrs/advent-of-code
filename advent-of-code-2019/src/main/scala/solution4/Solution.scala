package solution4

// TODO: rewrite this whole thing.
// TODO: Model your domain with MillionInt. What happens if it is lower or higher than 6 digits? Is it still pure?
// TODO: Override %, <, /.
// TODO: Think about cost savings by not looping through all of them.

case class MillionInt(integer: Int) {
  require(integer > 1000000-1)
  require(integer < 10000000-1)
}

object Solution {
  def differentPasswordsWithinRange2(range: List[MillionInt]): Int = {
    range.filter(hasExactlyTwoEqualAdjacentDigits)
      .filterNot(isDecreasingDigitsNumber)
      .size
  }

  private def getDigit(number: MillionInt): MillionInt = {
    number%10
  }

  private def dropDigit(number: MillionInt): MillionInt = {
    number/10
  }

  def hasExactlyTwoEqualAdjacentDigits(number: MillionInt): Boolean = {
    @scala.annotation.tailrec
    def loopNumberDigits(windowNumbers: List[MillionInt], number: MillionInt, foundTwoDigits: Boolean): Boolean = {
      if (number == 0) {
        (foundTwoDigits ||
          (windowNumbers(2) == windowNumbers(3) && windowNumbers(2)!= windowNumbers(1)) ||
          (windowNumbers.head != windowNumbers(1) && (windowNumbers(1) == windowNumbers(2)) && windowNumbers(2) != windowNumbers(3)))
      } else if (windowNumbers.head != windowNumbers(1)
        && windowNumbers(1) == windowNumbers(2)
        && windowNumbers(2) != windowNumbers(3)) {
        loopNumberDigits(windowNumbers.tail ::: List(getDigit(number)), dropDigit(number), true)
      } else {
        loopNumberDigits(windowNumbers.tail ::: List(getDigit(number)), dropDigit(number), foundTwoDigits)
      }
    }

    val windowNumbers = List(
      getDigit(number),
      getDigit(dropDigit(number)),
      getDigit(dropDigit(dropDigit(number))),
      getDigit(dropDigit(dropDigit(dropDigit(number)))),
    )
    val nextNumber = dropDigit(dropDigit(dropDigit(dropDigit(number))))
    val foundTwoDigits = (windowNumbers.head == windowNumbers(1)) && (windowNumbers(1) != windowNumbers(2))
    loopNumberDigits(windowNumbers, nextNumber, foundTwoDigits)
  }

  def differentPasswordsWithinRange1(range: List[MillionInt]): Int = {
    range.filter(hasAtLeastTwoEqualAdjacentDigits)
      .filterNot(isDecreasingDigitsNumber)
      .size
  }

  def hasAtLeastTwoEqualAdjacentDigits(number: MillionInt): Boolean = {
    val fn: (MillionInt, MillionInt) => Boolean = (a, b) => a == b
    loopNumberDigits(fn, getDigit(number), dropDigit(number))
  }

  @scala.annotation.tailrec
  def loopNumberDigits(fn: (MillionInt, MillionInt) => Boolean, currentDigit: MillionInt, number: MillionInt): Boolean = {
    val nextDigit = getDigit(number)
    if (number == 0) {
      false
    } else if (fn(currentDigit, nextDigit)) {
      true
    } else {
      loopNumberDigits(fn, nextDigit, dropDigit(number))
    }
  }

  def isDecreasingDigitsNumber(number: MillionInt): Boolean = {
    val fn: (MillionInt, MillionInt) => Boolean = (a, b) => a < b
    loopNumberDigits(fn, getDigit(number), dropDigit(number))
  }
}
