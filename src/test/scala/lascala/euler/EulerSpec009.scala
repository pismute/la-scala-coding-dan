package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec009 extends FunSpec with ShouldMatchers{
  it("should be 31875000") {
    @tailrec
    def euler0091(a:Long, b:Long):Long = (a, b, 1000L-a-b) match {
      case (a, b, c) if c > b && a * a + b * b == c * c => a*b*c
      case (_, b, _) if b >= 1000 => 0
      case _ => euler0091( a, b+1 )
    }

    @tailrec
    def euler009(a:Long):Long = euler0091(a, a+1) match {
      case 0 => euler009(a+1)
      case x => x
    }

    euler009(1) should be (31875000)
  }
}
