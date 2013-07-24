package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec006 extends FunSpec with ShouldMatchers{
  it("should be 25164150") {
    val natural = 1L to 100L
    val sumOfSquare = natural.foldLeft(0L)( (sum, n) => sum + n * n)
    val squareOfSum = natural.sum * natural.sum

    (squareOfSum - sumOfSquare) should be (25164150)
  }
}

