package lascala.codejam

/*
 * https://code.google.com/codejam/contest/1145485/dashboard
 */
object FreeCellStatistics {
  def gcd(a:Long, b:Long):Long= b match {
    case 0 => a
    case _ => gcd(b, a % b)
  }

  def solve(n:Long, pd:Long, pg:Long): String =
    (pd, pg) match {
      case (0, 0) => "Possible"
      case (_, 0) => "Broken"
      case (_, 100) if pd < 100 => "Broken"
      case _ =>
        val d = 100/gcd(pd, 100)

        if ( n >= d ) "Possible"
        else "Broken"
    }

  def process(lineIn: Iterator[String])(implicit lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      (lineIn.next split ' ' map(_.toLong)) match {
        case Array(n, pd, pg) =>
          lineOut(s"Case #$i: ${solve( n, pd, pg )}")
          //lineOut(s"Case #$i: $n, $pd, $pg: ${solve( n, pd, pg )}")
      }
    }

  def main(args: Array[String]) = {
    val aout = new java.io.PrintWriter("a.out")
    implicit val writer:String=>Unit = aout.println
    try {
      //process(io.Source.fromFile("A-small-practice.in").getLines)
      process(io.Source.fromFile("A-large-practice.in").getLines)
      //process(io.Source.fromFile("sample.in").getLines)(println)
    } finally {
      aout.flush(); aout.close()
    }
  }
}
