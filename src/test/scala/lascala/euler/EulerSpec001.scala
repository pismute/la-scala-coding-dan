package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec001 extends FunSpec with ShouldMatchers{
  it("should be 233168") {
    @tailrec
    def euler001(i:Int, accu:Int = 0):Int = i match{
      case 1000 => accu
      case x if ( x % 3 == 0 || i % 5 == 0 ) => euler001(i + 1, accu + i)
      case _ => euler001(i + 1, accu)
    }

    euler001(0) should be (233168)
  }
}

