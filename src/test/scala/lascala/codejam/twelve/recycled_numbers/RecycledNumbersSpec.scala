package lascala.codejam.twelve.recycled_numbers

import org.specs2.mutable._

class RecycledNumbersSpec extends Specification {
  def shift(s: String):String = s.tail + s.head

  def recycledNumbers(n: String) = {
    (1 until n.size).foldLeft(List[String]()) { (acc, _) =>
      shift(if (acc.isEmpty) n else acc.head) :: acc
      }.filter(_ > n)
      .filter(_.head != '0')
      .distinct
  }

  def pairs(a:Int, b:Int) = {
    for {
      n <- a to b
      m <- recycledNumbers(n.toString).map(_.toInt) if m <= b
    } yield (n, m)
  }

  "it" should {
    skipped
    "1 <= n < m <= 9" in {
      pairs(1, 9) must have size(0)
    }
    "10 <= n < m <= 40" in {
      pairs(10, 40) must have size(3)
    }
    "100 <= n < m <= 500" in {
      //println(recycledNumbers2("101"))
      //println(recycledNumbers("101"))
      //(pairs2(100, 500) diff pairs(100, 500)).foreach(println)
      pairs(100, 500) must have size(156)
    }
    "1111 <= n < m <= 2222" in {
      pairs(1111, 2222) must have size(287)
    }
  }

  "It" should {
    import scala.io.Source

    "small" in {
      val src = Source.fromURL(this.getClass.getResource("C-small-practice.in"))

      val lines = src.getLines().toList

      val cases = lines(0).toInt

      for( i <- 1 to cases){
        val c = lines(i).split(" ")
        println(c)
      }
      ok
    }
    "large" in {
      skipped
    }
  }
}
