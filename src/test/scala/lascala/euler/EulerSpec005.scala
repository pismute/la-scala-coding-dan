package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec005 extends FunSpec with ShouldMatchers{
  it("should be 232792560") {
    (2L to 20L).foldLeft(1L)( Euler.lcm(_, _) ) should be (232792560)
  }
}

