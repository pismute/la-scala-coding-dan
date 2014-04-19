package lascala.codejam.fourteen.qualification.c_minesweeper_master

import scala.collection.mutable
import org.specs2.mutable._
import scala.io.Source
import scalax.io.Resource
import scala.annotation.tailrec
import scala.util.Random

/*
 * failure
 */
class MinesweeperMasterSpec extends Specification {
  def main(lines:Iterator[String]) = {
    val cases = lines.next().toInt

    val output = Resource.fromFile("codejam.out")
    output.truncate(0)

    (1 to cases).foreach {
      (num) =>
        val game = lines.next().split(" ").map(_.toInt)
        output.write(s"Case #$num:\n")
        oneOf(game(0), game(1), game(2)) match {
          case Some(one) =>
            one.map(_.mkString("")).foreach{ (row)=>
              output.write(s"$row\n")
            }
          case None => output.write("Impossible\n")
        }
    }
  }

  def oneOf(r:Int, c:Int, m:Int):Option[Array[Array[Char]]] = {
    def isSweepable(r:Int, c:Int, m:Int) = {
      if( r == 1 || c == 1 ) r*c - m >= 2
      else r*c - m >= 4
    }

    def isSweepableOne(one:Array[Array[Char]]) = {
      def row() =
        (one.size-1 to 2 by -1)
        .map( (i)=> (one(i)(0), one(i)(1)) )
          .exists {
            case ('.', '*') => true
            case _ => false
          }
      def col() =
        (one(0).size-1 to 2 by -1)
        .map( (i)=> (one(0)(i), one(1)(i)) )
          .exists {
            case ('.', '*') => true
            case n => false
          }

      !(row || col)
    }

    if( isSweepable(r, c, m) ) {
      val one = Array.tabulate(r, c){
        case (0, 0) => 'c'
        case (_, _) => '.'
      }

      val minePos1 = for{
        i <- (r-1 to 2 by -1)
        j <- (c-1 to 0 by -1)
      } yield (i, j)

      val minePos2 =
        (c-1 to 2 by -1).flatMap{ i =>
          if ( r == 1) Array( (0, i) )
          else Array( (1, i), (0, i) )
        }

      val minePos = minePos1 ++ minePos2

      minePos.take(m).foreach{ case (i, j) => one(i)(j) = '*' }

      if( r == 1 || c == 1 || isSweepableOne(one)) Some(one) else None
    }else {
      None
    }

 }

  def printlnOne(one:Array[Array[Char]]) = one.map(_.mkString("")).foreach(println)

  "Samples" should {
    "pass 1" in {
      skipped
      oneOf(5, 5, 23) must beNone
    }
    "pass 2" in {
      skipped
      oneOf(3, 1, 1).foreach(printlnOne)
      oneOf(3, 1, 1) must beSome
    }
    "pass 3" in {
      skipped
      oneOf(2, 2, 1).foreach(printlnOne)
      oneOf(2, 2, 1) must beNone
    }
    "pass 4" in {
      skipped
      oneOf(4, 7, 3).foreach(printlnOne)
      oneOf(4, 7, 3) must beSome
    }
    "pass 5" in {
      skipped
      oneOf(10, 10, 82).foreach(printlnOne)
      oneOf(10, 10, 82) must beSome
    }
    "pass 6" in {
      skipped
      oneOf(2, 5, 1).foreach(printlnOne)
      oneOf(2, 5, 1) must beNone
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
      val lines = Source.fromURL(this.getClass.getResource("C-small-practice.in")).getLines()
      main(lines)
      ok
    }
    "pass large" in {
      skipped
      val lines = Source.fromURL(this.getClass.getResource("C-large.in")).getLines()
      main(lines)
      ok
    }
  }
}
