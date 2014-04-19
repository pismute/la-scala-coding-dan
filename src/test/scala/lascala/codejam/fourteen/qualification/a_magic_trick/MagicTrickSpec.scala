package lascala.codejam.fourteen.qualification.a_magic_trick

import org.specs2.mutable._
import scala.annotation.tailrec
import scala.io.Source
import scalax.io.{Output, Resource}

class MagicTrickSpec extends Specification {
  def getChosens(i:Iterator[String]) = {
    val pos = i.next().toInt
    (1 until pos).foreach( (_)=> i.next() )
    val ret = i.next().split(" ").map(_.toInt).sorted
    (pos until 4).foreach( (_)=> i.next() )

    ret.toList
  }

  @tailrec
  final def resolve(i:List[Int], j:List[Int], acc:List[Int] = Nil):List[Int] = (i, j) match {
    case (ih :: ii, jh :: jj) if ih == jh => resolve( ii, jj, ih :: acc)
    case (ih :: ii, jh :: jj) if ih < jh => resolve( ii, j, acc)
    case (ih :: ii, jh :: jj) if ih > jh => resolve( i, jj, acc)
    case _ => acc
  }

  def toY(ys: List[Int]) = (ys) match {
    case y :: Nil => y
    case y :: rest => "Bad magician!"
    case _ => "Volunteer cheated!"
  }

  def main(lines:Iterator[String]) = {
    val cases = lines.next().toInt

    val output = Resource.fromFile("codejam.out")
    output.truncate(0)

    (1 to cases).foreach {
      (num) =>
        val rowOfChosen1 = getChosens(lines)
        val rowOfChosen2 = getChosens(lines)

        output.write(s"Case #$num: ${toY(resolve(rowOfChosen1, rowOfChosen2))}\n")
    }
  }

  "It" should {
    "pass sample" in {
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
  }
}
