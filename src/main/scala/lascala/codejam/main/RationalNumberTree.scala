package lascala.codejam.main

/*
 * https://code.google.com/codejam/contest/2924486/dashboard#s=p1
 */
object RationalNumberTree {

  def solve1(n:BigInt): (BigInt, BigInt) =
    ((BigInt(1), BigInt(1)) /: n.toString(2).tail) { case ((p, q), bit) =>
      if( bit == '0') (p, p+q) else (p+q, q)
    }

  def solve2(p:BigInt, q:BigInt): BigInt = {
    def solve2Acc(p:BigInt, q:BigInt, acc:List[Int] = Nil): String =
      if( p > q ) solve2Acc( p-q, q, 1 :: acc)
      else if( p < q ) solve2Acc( p, q-p, 0 :: acc)
      else acc.mkString

    val bits = solve2Acc(p, q)
    BigInt(2).pow( bits.size ) + BigInt(bits, 2)
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      //println( lineIn.next())
      val answer = lineIn.next() split ' ' match {
        case Array("1", n) =>
          val (p, q) = solve1( BigInt(n) )
          s"$p, $q"
        case Array("2", p, q) => solve2( BigInt(p), BigInt(q) )
      }

      lineOut(s"Case #$i: $answer")
    }

  def main(args: Array[String]) = {
    val writer = new java.io.PrintWriter("a.out")
    try {
      //process(io.Source.fromFile("B-small-practice.in").getLines)(writer.println)
      process(io.Source.fromFile("B-large-practice.in").getLines)(writer.println)
      //process(io.Source.fromFile("example.in").getLines)(println)
    } finally {
      writer.flush(); writer.close()
    }
  }
}
