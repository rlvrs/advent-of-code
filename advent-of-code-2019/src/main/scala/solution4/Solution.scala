package solution4

//TODO: rewrite this whole thing.

object Solution {
  def differentPasswordsWithinRange2(range: List[Int]): Int = {
    range.filter(hasExactlyTwoEqualAdjacentDigits)
      .filterNot(isDecreasingDigitsNumber)
      .size
  }

  private def getDigit(number: Int): Int = {
    number%10
  }

  private def dropDigit(number: Int): Int = {
    number/10
  }

  def hasExactlyTwoEqualAdjacentDigits(number: Int): Boolean = {
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
      getDigit(number),
      getDigit(dropDigit(number)),
      getDigit(dropDigit(dropDigit(number))),
      getDigit(dropDigit(dropDigit(dropDigit(number)))),
    )
    val nextNumber = dropDigit(dropDigit(dropDigit(dropDigit(number))))
    val foundTwoDigits = (windowNumbers.head == windowNumbers(1)) && (windowNumbers(1) != windowNumbers(2))
    loopNumberDigits(windowNumbers, nextNumber, foundTwoDigits)
  }

  def differentPasswordsWithinRange1(range: List[Int]): Int = {
    range.filter(hasAtLeastTwoEqualAdjacentDigits)
      .filterNot(isDecreasingDigitsNumber)
      .size
  }

  def hasAtLeastTwoEqualAdjacentDigits(number: Int): Boolean = {
    @scala.annotation.tailrec
    def loopNumberDigits(currentDigit: Int, number: Int): Boolean = {
      val nextDigit = getDigit(number)
      if (number == 0) {
        false
      } else if (currentDigit == nextDigit) {
        true
      } else {
        loopNumberDigits(nextDigit, dropDigit(number))
      }
    }

    loopNumberDigits(getDigit(number), dropDigit(number))
  }

  def isDecreasingDigitsNumber(number: Int): Boolean = {
    @scala.annotation.tailrec
    def loopNumberDigits(currentDigit: Int, number: Int): Boolean = {
      val nextDigit = getDigit(number)
      if (number == 0) {
        false
      } else if (currentDigit < nextDigit) {
        true
      } else {
        loopNumberDigits(nextDigit, dropDigit(number))
      }
    }

    loopNumberDigits(getDigit(number), dropDigit(number))
  }
}
