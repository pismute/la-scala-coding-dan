package lascala.codejam

/**
 * https://code.google.com/codejam/contest/2924486/dashboard#s=p2
 */
object Sorting {

  def solve(worths:List[Long]): List[Long] = {
    val (evens, odds) = worths.partition(_ % 2 == 0) match {
      case (es, os) => ( es.sorted.reverse, os.sorted)
    }

    ((List[Long](), evens, odds) /: worths) { case ((acc, es, os), w) =>
      if(w % 2 == 0) (es.head :: acc, es.tail, os)
      else (os.head :: acc, es, os.tail)
    } match {
      case (acc, _, _) => acc.reverse
    }
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val worths = lineIn.drop(1).next().split(' ').toList.map(_.toLong)

      lineOut(s"Case #$i: ${solve(worths).mkString(" ")}")
    }

  def main(args: Array[String]) = {
    val writer = new java.io.PrintWriter("a.out")
    try {
      //process(io.Source.fromFile("C-small-practice.in").getLines)(writer.println)
      //process(io.Source.fromFile("C-large-practice.in").getLines)(writer.println)
      process(io.Source.fromFile("example.in").getLines)(println)
    } finally {
      writer.flush(); writer.close()
    }
  }
}
