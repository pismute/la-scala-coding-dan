package lascala.codejam.fourteen.qualification.b_cookie_clicker_alpha

import org.specs2.mutable._
import scala.annotation.tailrec
import scala.io.Source
import scalax.io.Resource
import scala.collection.JavaConverters._

class CookieClickerAlphaSpec extends Specification {
  def main(lines:Iterator[String]) = {
    val cases = lines.next().toInt

    val output = Resource.fromFile("codejam.out")
    output.truncate(0)

    (1 to cases).foreach {
      (num) =>
        val args = lines.next().split(" ").map(_.toDouble)
        val oo = f"Case #$num: ${getMinOfSeconds(args(0), args(1), args(2))}%.7f\n"
        output.write(oo)
    }
  }

  /*
  case class Farmline(val c:Double, val f:Double, val t:Double, val tX:Double){
    def all():Double = t + tX
    override def toString() = s"c($c), f($f), t($t), tX($tX), all($all)"
  }

  def getMinOfSeconds(C:Double, F:Double, X:Double) = {
    def farmline:Stream[Farmline] =
      Farmline(0, 2, 0, X/2) #:: farmline.map {
        (pre) =>
        val f = pre.f + F
        Farmline(pre.c + 1, f, pre.t + C/pre.f, X/f)
      }

    //farmline.take(10).foreach(println)
    var min:Farmline = Farmline(0, 0, 0, Double.MaxValue)
    farmline.find{ (f)=>
      if( min.all < f.all ) true else { min = f; false }
    }

    Math.rint(min.all() * 10000000)/10000000
  }
  */

  def getMinOfSeconds(C:Double, F:Double, X:Double) = {
    @tailrec
    def findMin(c:Double=0, f:Double=2,
                t:Double=0, tX:Double=X/2,
                min:Double = X/2):Double =
      {
        val nextF = f + F
        val nextT = t + C/f
        val nextTX = X/nextF
        val nextMin = nextT + nextTX

        //println(s"c(${c+1}), nextF($nextF), nextT($nextT), tX($nextTX), nextMin($nextMin)")

        if( min < nextMin ) min
        else findMin(c + 1, nextF, nextT, nextTX, nextMin)
      }

    Math.rint(findMin() * 10000000)/10000000
  }

  "Samples" should {
    "pass 30.0 1.0 2.0" in {
      skipped
      this.getMinOfSeconds(30.0, 1.0, 2.0) must_==(1.0000000)
    }
    "pass 30.0, 2.0, 100.0" in {
      skipped
      this.getMinOfSeconds(30.0, 2.0, 100.0) must_==(39.1666667)
    }
    "pass 30.50000 3.14159 1999.19990" in {
      skipped
      this.getMinOfSeconds(30.50000, 3.14159, 1999.19990) must_==(63.9680013)
    }
    "pass 500.0 4.0 2000.0" in {
      skipped
      this.getMinOfSeconds(500.0, 4.0, 2000.0) must_==(526.1904762)
    }
  }

  "It" should {
    "pass sample" in {
      skipped
      val lines = Source.fromURL(this.getClass.getResource("sample.in")).getLines()
      main(lines)
      ok
    }
    "pass small" in {
      skipped
      val lines = Source.fromURL(this.getClass.getResource("B-small-practice.in")).getLines()
      main(lines)
      ok
    }
    "pass large" in {
      skipped
      val lines = Source.fromURL(this.getClass.getResource("B-large-practice.in")).getLines()
      main(lines)
      ok
    }
  }
}
