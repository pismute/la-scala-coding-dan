package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec002 extends FunSpec with ShouldMatchers{
  it("should be 4613732") {
    @tailrec
    def finonacci(a:Int, b:Int, accu:Int = 0):Int = (a, b) match {
      case (x, y) if ( y > 4000000 ) => accu
      case (x, y) if ( y % 2 == 0 ) => finonacci(y, x + y, accu + y)
      case (x, y) => finonacci(y, x + y, accu)
    }

    finonacci(1, 2) should be (4613732)
  }
}

