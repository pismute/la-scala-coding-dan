package lascala.codejam

/*
 * https://code.google.com/codejam/contest/2924486/dashboard#s=p4
 */
import scala.collection.mutable.PriorityQueue

object SpaceshipDefence {
  case class Edge(a:String, b:String, t:Int)

  def solve(rooms:Vector[String], lifts: List[Edge], soldiers: Array[(String, String)]): Array[Int] = {
    val graph = lifts.toSet[Edge].groupBy(_.a)

    def findSortestPath(a:String, b:String): Int = {
      val pq = new PriorityQueue[(Int, String)].reverse //ascending
      pq += ((0, a))

      def loop(done:Set[String]):Int =
        if( pq.isEmpty) -1
        else pq.dequeue match {
          case (w, room) if room == b => w
          case (w, room) if !done.contains(room) =>
            graph.get(room).foreach {//option
              _.foreach {//set
                case Edge(a, b, t) => pq += ((w + t, b))
              }
            }

            loop(done + room)
          case _ => loop(done)
        }

      loop(Set.empty)
    }

    soldiers.map{ case (s, e) => findSortestPath(s, e) }
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next.toInt) {
      val n = lineIn.next.toInt //rooms
      val rooms = Vector("") ++ Vector.fill(n)(lineIn.next)
      val m = lineIn.next.toInt //turbolifts
      val lifts = List.fill(m)(lineIn.next.split(" ").map(_.toInt)).map{
        case Array(a, b, t) => Edge(rooms(a), rooms(b), t)
      }
      val s = lineIn.next.toInt //soldiers
      val soldiers = Array.fill(s) {
        lineIn.next.split(" ").map(_.toInt) match {
          case Array(s, e) => (rooms(s), rooms(e))
        }
      }

      lineOut(s"Case #$i: \n${solve(rooms, lifts, soldiers).mkString("\n")}")
    }

  def main(args: Array[String]) = {
    val writer = new java.io.PrintWriter("a.out")
    try {
      //process(io.Source.fromFile("E-small-practice.in").getLines)(writer.println)
      process(io.Source.fromFile("E-large-practice.in").getLines)(writer.println)
      //process(io.Source.fromFile("sample.in").getLines)(println)
    } finally {
      writer.flush(); writer.close()
    }
  }
}
