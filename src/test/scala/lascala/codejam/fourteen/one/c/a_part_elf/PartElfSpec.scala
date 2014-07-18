package lascala.codejam.fourteen.one.c.a_part_elf

import org.specs2.mutable._
import scala.io.Source
import scalax.io.Resource
import scala.annotation.tailrec

class PartElfSpec extends Specification {
  def main(lines:Iterator[String]) = {
    val cases = lines.next().toInt

    val output = Resource.fromFile("codejam.out")
    output.truncate(0)

    (1 to cases).foreach {
      (num) =>
        val List(x, y) = lines.next().split("/").map(_.toLong).toList
        val r = Rational(x, y)
        output.write(s"Case #$num: ${toY(r)}\n")
    }
  }

  def toY(r:Rational) =
    if ( isPowOf2(r.denom) ) solve(r)
    else "impossible"

  case class Rational(x: Long, y: Long) {
    private def gcd(a: Long, b: Long): Long =
      if (b == 0) a else gcd(b, a % b)

    val numer = x / gcd(x, y)
    val denom = y / gcd(x, y)

    def le(that: Rational) =
      numer * that.denom <= that.numer * denom

    override def toString = s"$numer/$denom"
  }

  @tailrec
  private def isPowOf2(n:Long):Boolean =
    if (n == 1) true
    else if( n % 2 == 0) isPowOf2( n / 2 )
    else false

  def solve(r: Rational):Long = {

    //println(s"r:$r")
    @tailrec
    def find(i: Long= 1):Long = {
      val generation = Rational(1, Math.pow(2, i).toLong)
      //println(generation)
      if( generation.le(r) ) i
      else find(i + 1)
    }

    find()
  }

  "Samples" should {
    "pass 1" in {
      skipped
      solve(Rational(1,2)) must be_==(1)
      solve(Rational(3,4)) must be_==(1)
      solve(Rational(1,4)) must be_==(2)
      isPowOf2(23) must be_==(false)
      solve(Rational(123,31488)) must be_==(8)
      ok
    }
    "pass for large" in {
      skipped
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
      val lines = Source.fromURL(this.getClass.getResource("A-small-attempt0.in")).getLines()
      main(lines)
      ok
    }
    "pass large" in {
      val lines = Source.fromURL(this.getClass.getResource("A-large.in")).getLines()
      main(lines)
      ok
    }
  }
}

