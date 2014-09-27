package lascala.codejam

/*
 * https://code.google.com/codejam/contest/975485/dashboard#s=p1
 */
object Magicka {

  def solve(cs:Seq[String], ds:Seq[String], bases:String): String = {
    val csm = cs.map( (s) => Set(s.head, s.tail.head) -> s.tail.tail.head ).toMap
    val dsm = ds.map( (s) => Set(s.head, s.tail.head) -> true).toMap

    def invoke(bases:List[Char], stack: List[Char], acc: List[Char]):List[Char] =
      bases match {
        case Nil => stack ::: acc
        case b :: bs if stack.isEmpty =>
          invoke(bs, b :: stack, acc)
        case b :: bs if csm.contains(Set(stack.head, b)) =>
          invoke(bs, csm(Set(stack.head, b)) :: stack.tail, acc)
        case b :: bs if stack.exists( (e) => dsm.contains(Set(e, b))) =>
          invoke(bs, Nil, Nil)
        case b :: bs =>
          invoke(bs, b :: stack, acc)
      }

    "[" + invoke(bases.toList, Nil, Nil).reverse.mkString(", ") + "]"
  }

  def process(lineIn: Iterator[String])(implicit lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val it = lineIn.next().split(" ").iterator
      val nCs = it.next().toInt
      val cs = Seq.fill(nCs)(it.next())
      val nDs = it.next().toInt
      val ds = Seq.fill(nDs)(it.next())
      it.next()
      val bases = it.next()
      lineOut(s"Case #$i: ${solve( cs, ds, bases)}")
    }

  def main(args: Array[String]) = {
    val aout = new java.io.PrintWriter("a.out")
    implicit val writer:String=>Unit = aout.println
    try {
      //process(io.Source.fromFile("B-small-practice.in").getLines)
      process(io.Source.fromFile("B-large-practice.in").getLines)
      //process(io.Source.fromFile("sample.in").getLines)(println)
    } finally {
      aout.flush(); aout.close()
    }
  }
}
