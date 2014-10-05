import scala.collection.mutable

/*
 * https://code.google.com/codejam/contest/1781488/dashboard
 */
object DiamondInheritance {

  def solve(cs: Seq[(Int, Seq[Int])]): String = {
    val leaves:Set[Int] = (1 to cs.size).toSet -- cs.flatMap(_._2).toSet
    val supers = cs.toMap

    def dfs(leaf:Int, visited: mutable.Set[Int]): Boolean =
      visited(leaf) || {
        visited += leaf
        if(supers(leaf).isEmpty ) false
        else supers(leaf).exists( (s) =>
          visited(s) || dfs(s, visited) )
      }

    leaves.exists(dfs(_, mutable.Set())) match {
      case true => "Yes"
      case false => "No"
    }
  }

  def process(lineIn: Iterator[String])(implicit lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val n = lineIn.next().toInt
      val tree = for(c <- (1 to n).toSeq) yield {
        val supers = lineIn.next().split(' ').drop(1).map(_.toInt).toSeq
        (c, supers)
      }

      lineOut(s"Case #$i: ${solve( tree )}")
    }

  def main(args: Array[String]) = {
    val aout = new java.io.PrintWriter("a.out")
    implicit val writer:String=>Unit = aout.println
    try {
      //process(io.Source.fromFile("A-small-practice.in").getLines)
      //process(io.Source.fromFile("A-large-practice.in").getLines)
      process(io.Source.fromFile("sample.in").getLines)(println)
    } finally {
      aout.flush(); aout.close()
    }
  }
