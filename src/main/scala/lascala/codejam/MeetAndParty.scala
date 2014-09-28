package lascala.codejam

import scala.collection.Searching._

/*
 * https://code.google.com/codejam/contest/2929486/dashboard#s=p1
 *
 * enshahar님 해설
 * https://github.com/enshahar/Study/blob/master/CodeJam_MeetAndParty.md
 *
 */
object MeetAndParty {
  def solve(areas: Seq[Array[Long]]): String = {
    val houses:Seq[(Long,Long)] = areas.flatMap{ case Array(x1, y1, x2, y2) =>
      for{
        x <- (x1 to x2).toSeq
        y <- y1 to y2
      } yield (x, y)
    }

    val (xs, ys) = houses.unzip match { case (xxs, yys) => (xxs.sorted.toIndexedSeq, yys.sorted.toIndexedSeq)}

    // if it not cached sum, large data would be  never ended
    val sumx = (List(0L) /: xs){ (acc, x) => (x + acc.head) :: acc }.reverse.toArray
    val sumy = (List(0L) /: ys){ (acc, x) => (x + acc.head) :: acc }.reverse.toArray

    val last = houses.size
    val house:(Long, Long, Long) = houses.map{ case (x,y) =>
      val xi = xs.search(x) match { case Found(v) => v + 1 }
      val yi = ys.search(y) match { case Found(v) => v + 1 }

      (xi*x - sumx(xi) + (sumx(last) - sumx(xi)) - (last - xi)*x +
        yi*y - sumy(yi) + (sumy(last) - sumy(yi)) - (last - yi)*y, x, y)

    }.sorted.head

    s"${house._2} ${house._3} ${house._1}"
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val b = lineIn.next().toInt

      val areas = Seq.fill(b)( lineIn.next().split(' ').map(_.toLong) )

      lineOut(s"Case #$i: ${solve(areas)}")
    }

  def main(args: Array[String]) = {
    val writer = new java.io.PrintWriter("a.out")
    try {
      //process(io.Source.fromFile("B-small-practice.in").getLines)(writer.println)
      process(io.Source.fromFile("B-large-practice.in").getLines)(writer.println)
      //process(io.Source.fromFile("sample.in").getLines)(println)
    } finally {
      writer.flush(); writer.close()
    }
  }
}
