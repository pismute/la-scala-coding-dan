package lascala.codejam

/*
 * https://code.google.com/codejam/contest/2434486/dashboard
 */
object Osmos {

  // return (ops, nextArmin)
  def addOps(armin:Long, mote:Long, ops:Long=0): (Long, Long) = {
    if( armin > mote ) (ops, armin)
    else addOps( armin + (armin -1), mote, ops +1)
  }

  def solve(armin:Long, motes:IndexedSeq[Long])(implicit outln: String => Unit): Long = {
    val size = motes.size

    def absorb(armin:Long, i:Int, ops:Long=0):Long = {
      val nRest = size - i

      if( nRest < 1 ) ops
      else if( motes(i) < armin )
        absorb(armin + motes(i), i+1, ops)
      else {
        val (n, next) = addOps(armin, motes(i))

        if( ops + n >= size) size
        else if( n < nRest)
          absorb( next, i, ops + n)
        else ops + nRest
      }
    }

    if( armin == 1 ) motes.size
    else absorb(armin, 0, 0)
  }

  def process(lineIn: Iterator[String])(implicit outln: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val Array(armin, n) = lineIn.next().split(' ').map(_.toLong)
      val motes = lineIn.next().split(' ').map(_.toLong).sorted.toIndexedSeq

      outln(s"Case #$i: ${solve(armin, motes)}")
    }

  def main(args: Array[String]) = {
    val aout = new java.io.PrintWriter("a.out")
    implicit val outln:String=>Unit = aout.println

    try {
      //process(io.Source.fromFile("A-small-practice.in").getLines)
      process(io.Source.fromFile("A-large-practice.in").getLines)
      //process(io.Source.fromFile("sample.in").getLines)(println)
    } finally {
      aout.flush(); aout.close()
    }
  }
}
