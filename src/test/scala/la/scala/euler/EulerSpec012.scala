package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec012 extends FunSpec with ShouldMatchers{
  it("should be 76576500") {
    lazy val triangleNumbers =
      Stream.from(1)
        .map(n=> n*(n+1)/2)

    /* from http://pavelfatin.com/scala-for-project-euler/ 
    def countFactors(t: Int) = Range(1, Int.MaxValue)
      .takeWhile(n => n * n <= t) //??
      foldLeft(0)((s, n) => if(t % n == 0) s + 2 else s) //??

    triangleNumbers.find(countFactors(_) > 500).get should be (76576500)
    */
  }
}

