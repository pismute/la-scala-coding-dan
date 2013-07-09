package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec004 extends FlatSpec with ShouldMatchers{
  it should "be 906609" in {
    (for {
      i <- 100 until 1000
      j <- 100 until 1000
      k = i * j
      if Euler.isPalindrome( k.toString )
    } yield k).max should be (906609)
  }
}

