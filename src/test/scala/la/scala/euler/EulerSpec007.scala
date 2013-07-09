package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec007 extends FlatSpec with ShouldMatchers{
  it should "be 104743" in {
    Euler.nthPrime(10001) should be (104743)
  }
}

