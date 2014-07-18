package lascala.codejam.twelve_korea.a_new_calendar

import org.specs2.mutable._
import scala.io.Source
import scalax.io.Resource
import scala.annotation.tailrec
import scala.util.Random

class NewCalendarSpec extends Specification {
  def main(lines:Iterator[String]) = {
    val cases = lines.next().toInt

    val output = Resource.fromFile("codejam.out")
    output.truncate(0)

    (1 to cases).foreach {
      (num) =>
        val input = lines.next().split(" ").map(_.toLong)
        val count = countLines(input(0), input(1), input(2))
        output.write(s"Case #$num: ${count}\n")
    }
  }

  def countLines(M:Long, D:Long, W:Long):Long = {
    @tailrec
    def count(m:Long, mode:Long, acc:Long):Long = m match {
      case 0 => acc
      case _ =>
        val days = mode + D
        //println(s"m=$m, mode=$mode, days=$days, acc=$acc")
        count(m-1, days % W,acc + Math.ceil( days / W.toDouble ).toLong )
    }

    @tailrec
    def getCycle(mode:Long, acc:Long=1):Long = mode match {
      case 0 => acc
      case _ =>
        getCycle( (mode + D)%W, acc+1 )
    }

    val monthPerCycle = getCycle(D%W)
    val linesPerCycle = count(monthPerCycle, 0, 0)
    val cycles = M/monthPerCycle
    val linesPerFullCycle = linesPerCycle * cycles
    val linesPerRest = count(M%monthPerCycle, 0, 0)

    //println (s"M=$M, D=$D, W=$W")
    //println (monthPerCycle, linesPerCycle, cycles, linesPerFullCycle, linesPerRest)
    linesPerFullCycle + linesPerRest
  }

  "Samples" should {
    "pass 1" in {
      skipped
      countLines(3, 11, 4) must_==(11)
      ok
    }
    "pass 2" in {
      skipped
      countLines(12, 28, 7) must_==(48)
      ok
    }
    "pass 3" in {
      skipped
      countLines(10, 35, 10) must_==(40)
      ok
    }
    "pass 4" in {
      skipped
      countLines(786674420701L, 308049, 31) must_==(7817995769520754L)
      ok
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
      val lines = Source.fromURL(this.getClass.getResource("A-small-practice.in")).getLines()
      main(lines)
      ok
    }
    "pass large" in {
      skipped
      val lines = Source.fromURL(this.getClass.getResource("A-large-practice.in")).getLines()
      main(lines)
      ok
    }
  }
}
