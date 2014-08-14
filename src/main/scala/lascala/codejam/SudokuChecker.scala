package lascala.codejam

/*
 * https://code.google.com/codejam/contest/2929486/dashboard
 */
object SudokuChecker {
  type Matrix = Vector[Vector[Int]]

  def squareGame(bx: Int, by: Int, n: Int, matrix: Matrix): Set[Int] = {
    for {
      i <- (0 until n).toSet[Int]
      j <- (0 until n)
    } yield {
      matrix(bx + i)(by + j)
    }
  }

  def squareGames(n: Int, matrix: Matrix): List[Set[Int]] = {
    for {
      i <- (0 until n).toList
      j <- (0 until n)
    } yield squareGame(i * n, j * n, n, matrix)
  }

  def horizontalGames(n: Int, matrix: Matrix): List[Set[Int]] =
    (0 until n * n).map(matrix(_).toSet).toList

  def verticalGames(n: Int, matrix: Matrix): List[Set[Int]] =
    (for (i <- (0 until n * n)) yield {
      for (j <- (0 until n * n).toSet[Int]) yield matrix(j)(i)
    }).toList

  def games(n: Int, matrix: Matrix): List[Set[Int]] =
    horizontalGames(n, matrix) ++ verticalGames(n, matrix) ++ squareGames(n, matrix)

  def solve(n: Int, matrix: Matrix): String = {
    val nn = n * n
    val answer = (1 to nn).toSet

    games(n, matrix).find(g => g.size != nn || g.diff(answer).size > 0) match {
      case None => "Yes"
      case _ => "No"
    }
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next.toInt) {
      val n = lineIn.next.toInt //rooms
      val matrix = Vector.fill(n * n)(lineIn.next().split(" ").map(_.toInt).toVector)

      lineOut(s"Case #$i: ${solve(n, matrix)}")
    }

  def main(args: Array[String]) = {
    val writer = new java.io.PrintWriter("a.out")
    try {
      //process(io.Source.fromFile("A-small-practice.in").getLines)(writer.println)
      process(io.Source.fromFile("A-large-practice.in").getLines)(writer.println)
      //process(io.Source.fromFile("sample.in").getLines)(println)
    } finally {
      writer.flush(); writer.close()
    }
  }
}
