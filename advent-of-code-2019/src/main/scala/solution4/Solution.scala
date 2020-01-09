package solution4

import scala.annotation.tailrec

// TODO: rewrite this whole thing.
// TODO: Model your domain with MillionInt. What happens if it is lower or higher than 6 digits? Is it still pure?
// TODO: Think about cost savings by not looping through all of them.

case class SixDigitInt(integer: Int) {
  require(integer > 100000-1)
  require(integer < 1000000-1)
}

object Solution {
  def differentPasswordsWithinRange2(range: List[SixDigitInt]): Int = {
    range.filter(hasExactlyTwoEqualAdjacentDigits)
      .filterNot(isDecreasingDigitsNumber)
      .size
  }

  private def getDigit(number: Int): Int = {
    number % 10
  }

  private def dropDigit(number: Int): Int = {
    number / 10
  }

  def hasExactlyTwoEqualAdjacentDigits(number: SixDigitInt): Boolean = {
    @scala.annotation.tailrec
    def loopNumberDigits(windowNumbers: List[Int], number: Int, foundTwoDigits: Boolean): Boolean = {
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
      getDigit(number.integer),
      getDigit(dropDigit(number.integer)),
      getDigit(dropDigit(dropDigit(number.integer))),
      getDigit(dropDigit(dropDigit(dropDigit(number.integer)))),
    )
    val nextNumber = dropDigit(dropDigit(dropDigit(dropDigit(number.integer))))
    val foundTwoDigits = (windowNumbers.head == windowNumbers(1)) && (windowNumbers(1) != windowNumbers(2))
    loopNumberDigits(windowNumbers, nextNumber, foundTwoDigits)
  }

  def differentPasswordsWithinRange1(range: List[SixDigitInt]): Int = {
    range.filter(hasAtLeastTwoEqualAdjacentDigits)
      .filterNot(isDecreasingDigitsNumber)
      .size
  }

  def hasAtLeastTwoEqualAdjacentDigits(number: SixDigitInt): Boolean = {
    val fn: (Int, Int) => Boolean = (a, b) => a == b
    loopNumberDigits(fn, getDigit(number.integer), dropDigit(number.integer))
  }

  @tailrec
  private def loopNumberDigits(fn: (Int, Int) => Boolean, currentDigit: Int, number: Int): Boolean = {
    val nextDigit = getDigit(number)
    if (number == 0) {
      false
    } else if (fn(currentDigit, nextDigit)) {
      true
    } else {
      loopNumberDigits(fn, nextDigit, dropDigit(number))
    }
  }

  def isDecreasingDigitsNumber(number: SixDigitInt): Boolean = {
    val fn: (Int, Int) => Boolean = (a, b) => a < b
    loopNumberDigits(fn, getDigit(number.integer), dropDigit(number.integer))
  }
}
