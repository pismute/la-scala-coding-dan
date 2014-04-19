package lascala.codejam.fourteen.qualification.d_deceitful_war

import org.specs2.mutable._
import scala.io.Source
import scalax.io.Resource
import scala.annotation.tailrec
import scala.util.Random

class DeceitfulWarSpec extends Specification {
  def main(lines:Iterator[String]) = {
    val cases = lines.next().toInt

    val output = Resource.fromFile("codejam.out")
    output.truncate(0)

    (1 to cases).foreach {
      (num) =>
        val naoKen = lines.next()
        val naomi = lines.next().split(" ").map(_.toDouble).sorted.toList
        val ken = lines.next().split(" ").map(_.toDouble).sorted.toList
        val deceitfulWin =
          this.makeDeceitfulRound(naomi.reverse, ken.reverse, naomi.size - 1).count(_.didNaomiWin)
        val win =
          this.makeRound(naomi, ken, naomi.size - 1).count(_.didNaomiWin)
        output.write(s"Case #$num: $deceitfulWin $win\n")
    }
  }

  case class Round(n:Double, k:Double){
    def didNaomiWin = n > k
  }

  @tailrec
  final def makeRound(naomi:List[Double], ken:List[Double], last:Int,
                acc:List[Round]= Nil):List[Round] = (ken, naomi) match {
    case (_, Nil) => acc
    case (Nil, _) => acc
    case (k :: ke, n :: na) =>
      if (k > n) makeRound(na, ke, last - 1, Round(n, k) :: acc)
      else {
        makeRound(naomi, ke, last - 1, Round( naomi(last) , k) :: acc)
      }
  }

  @tailrec
  final def makeDeceitfulRound(naomi:List[Double], ken:List[Double], last:Int,
                      acc:List[Round]= Nil):List[Round] = (ken, naomi) match {
    case (_, Nil) => acc
    case (Nil, _) => acc
    case (k :: ke, n :: na) =>
      if (k < n) makeDeceitfulRound(na, ke, last - 1, Round(n, k) :: acc)
      else {
        makeDeceitfulRound(naomi, ke, last - 1, Round( naomi(last) , k) :: acc)
      }
  }

  "Samples" should {
    "pass 1" in {
      skipped
      val naomi = List(0.5).sorted
      var ken = List(0.6).sorted

      makeRound(naomi, ken, naomi.size - 1).count(_.didNaomiWin) must_==(0)
      makeDeceitfulRound(naomi.reverse, ken.reverse, naomi.size - 1).count(_.didNaomiWin) must_==(0)
      ok
    }
    "pass 2" in {
      skipped
      val naomi = List(0.7, 0.2).sorted
      var ken = List(0.8, 0.3).sorted

      makeRound(naomi, ken, naomi.size - 1).count(_.didNaomiWin) must_==(0)
      makeDeceitfulRound(naomi.reverse, ken.reverse, naomi.size - 1).count(_.didNaomiWin) must_==(1)
      ok
    }
    "pass 3" in {
      skipped
      val naomi = List(0.5, 0.1, 0.9).sorted
      var ken = List(0.6, 0.4, 0.3).sorted

      makeRound(naomi, ken, naomi.size - 1).count(_.didNaomiWin) must_==(1)
      makeDeceitfulRound(naomi.reverse, ken.reverse, naomi.size - 1).count(_.didNaomiWin) must_==(2)
      ok
    }
    "pass 4" in {
      skipped
      val naomi = List(0.186, 0.389, 0.907, 0.832, 0.959, 0.557, 0.300, 0.992, 0.899).sorted
      var ken = List(0.916, 0.728, 0.271, 0.520, 0.700, 0.521, 0.215, 0.341, 0.4582).sorted

      makeRound(naomi, ken, naomi.size - 1).count(_.didNaomiWin) must_==(4)
      makeDeceitfulRound(naomi.reverse, ken.reverse, naomi.size - 1).count(_.didNaomiWin) must_==(8)
      ok
    }
    "pass for large" in {
      skipped
      (0 to 50).foreach { (_) =>
        val rand = List.fill(2000)(Random.nextDouble).distinct
        rand must have size(2000)

        val naomi = rand.take(1000).sorted
        var ken = rand.drop(1000).sorted

        val round = makeRound(naomi, ken, naomi.size - 1).count(_.didNaomiWin)
        val deceitfulRound = makeDeceitfulRound(naomi.reverse, ken.reverse, naomi.size - 1).count(_.didNaomiWin)
        println(s"$round, $deceitfulRound")
      }
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
      val lines = Source.fromURL(this.getClass.getResource("D-small-attempt0.in")).getLines()
      main(lines)
      ok
    }
    "pass large" in {
      val lines = Source.fromURL(this.getClass.getResource("D-large.in")).getLines()
      main(lines)
      ok
    }
  }
}
