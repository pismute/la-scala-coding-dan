package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec010 extends FlatSpec with ShouldMatchers{
  it should "be 142913828922" in {
    (1L to 2000000L)
      .filter( Euler.isPrime )
      .sum should be (142913828922L)
  }
}

