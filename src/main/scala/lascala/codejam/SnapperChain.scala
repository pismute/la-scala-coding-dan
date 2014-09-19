package lascala.codejam

/*
 * https://code.google.com/codejam/contest/433101/dashboard
 */
object SnapperChain {

  def solve(n:Int, k:Int): String = {
    BigInt(k).toString(2).takeRight(n).padTo(n,'0').find( _ == '0') match {
      case None => "ON"
      case _ => "OFF"
    }
  }

  def process(lineIn: Iterator[String])(implicit lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val Array(n,k) = lineIn.next().split(' ').map(_.toInt)
      lineOut(s"Case #$i: ${solve( n, k)}")
    }

  def main(args: Array[String]) = {
    val aout = new java.io.PrintWriter("a.out")
    implicit val writer:String=>Unit = aout.println
    try {
      //process(io.Source.fromFile("A-small-practice.in").getLines)
      process(io.Source.fromFile("A-large-practice.in").getLines)
      //process(io.Source.fromFile("example.in").getLines)(println)
    } finally {
      aout.flush(); aout.close()
    }
  }
}
