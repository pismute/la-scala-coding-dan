package lascala.codejam

/**
 * https://code.google.com/codejam/contest/2924486/dashboard#s=p3
 */
object CrossTheMage {
  object Direction extends Enumeration {
    type Direction = Value
    val N, E, S, W = Value

    val N_LFRB:List[Neighbor] = (0, -1, W) :: (-1, 0, N) :: (0, 1, E) :: (1, 0, S) :: Nil

    def lfrb(d:Direction) = N_LFRB.drop(d.id) ++ N_LFRB.take(d.id)

    def neighbors(x: Int, y: Int, d:Direction): List[Neighbor] =
      lfrb(d).map{ case (xx, yy, d) => (x + xx, y + yy, d) }
  }

  import Direction._
  type Neighbor = (Int, Int, Direction)

  // abstract class Direction {
  //   def LFRB:List[Neighbor]
  //   def rotateRight(lfrb: List[Neighbor]) = lfrb.tail ++ List(lfrb.head)
  //   def neighbors(x: Int, y: Int): List[Neighbor] =
  //     LFRB.map{ case (xx, yy, d) => (x + xx, y + yy, d) }
  // }

  // object N extends Direction {
  //   lazy val LFRB:List[Neighbor] = (0, -1, W) :: (-1, 0, N) :: (0, 1, E) :: (1, 0, S) :: Nil
  //   override def toString() = "N"
  // }

  // object E extends Direction {
  //   lazy val LFRB = rotateRight(N.LFRB)
  //   override def toString() = "E"
  // }

  // object S extends Direction {
  //   lazy val LFRB = rotateRight(E.LFRB)
  //   override def toString() = "S"
  // }

  // object W extends Direction {
  //   lazy val LFRB = rotateRight(S.LFRB)
  //   override def toString() = "W"
  // }

  def solve(m:Array[String], sx:Int, sy:Int, ex:Int, ey:Int): String = {
    def isWall(x: Int, y: Int) =
      x < 0 || m.size <= x || y < 0 || m(0).size <= y || m(x)(y) == '#'

    def solveAcc(x:Int, y:Int, d:Direction, acc:List[Direction] = Nil, tries:Int = 0):List[Direction] =
      neighbors(x, y, d).find{ case (xx, yy, dd) =>
        // 첫번째 노트는 그냥 LFRB 순으로 빈자리로 들어가면 된다.
        //    .
        // .^ ^ ^. ^ <= 요 모양중에서 `.`인 것을 찾는다.
        //         .
        !isWall(xx, yy)
      } match {
        case _ if tries > 10000 => Nil
        case None => Nil
        case Some((xx, yy, dd)) =>
          if(xx == ex && yy == ey) dd :: acc
          else solveAcc(xx, yy, dd, dd :: acc, tries + 1)
      }

    val ns = neighbors(sx, sy, N)
    (ns.last :: ns).sliding(2)
      .find { case List((x1, y1, _), (x2, y2, _)) =>
        // 두번째 노트를 해결하기 위해 초기 방향을 찾는다.
        //     . #
        // .^ #^ ^. ^# <= 요 모양중에서 `.`인 것을 찾는다.
        //  #       .     그래서 초기 방향을 설정한다.
        isWall(x1, y1) && !isWall(x2, y2)
      }.map {
        case List(_, (_, _, d)) => d //Direction으로 변환
      } match {
        case None => "" // 꽉 막혀서 초기 방향를 못 찾음, ran out of energy
        case Some(d) => // 초기 방향 찾음.
          solveAcc(sx, sy, d).reverse.mkString
      }
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val n = lineIn.next().toInt
      val m = Array.fill(n)(lineIn.next())
      val Array(sx, sy, ex, ey) = lineIn.next().split(' ').map(_.toInt)
      val steps =solve(m, sx - 1, sy- 1, ex - 1 , ey - 1)
      val output =
        if( steps.isEmpty) "Edison ran out of energy."
        else s"${steps.size}\n$steps"

     lineOut(s"Case #$i: $output")
    }

  def main(args: Array[String]) = {
    val writer = new java.io.PrintWriter("a.out")
    try {
      //process(io.Source.fromFile("D-small-practice.in").getLines)(writer.println)
      //process(io.Source.fromFile("D-large-practice.in").getLines)(writer.println)
      process(io.Source.fromFile("sample.in").getLines)(println)
    } finally {
      writer.flush(); writer.close()
    }
  }
}
