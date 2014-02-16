package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec003 extends FunSpec with ShouldMatchers{
  it("should be 6857") {
    Euler.getLargestPrime(600851475143L) should be (6857)
  }
}

