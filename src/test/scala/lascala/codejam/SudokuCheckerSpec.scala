package lascala.codejam

import org.specs2.mutable._

object SudokuCheckerSpec extends Specification {

  import SudokuChecker._

  def matrix(s:String) = s.lines.map(_.split(" ").map(_.toInt).toVector).toVector
  val matrix1 = matrix("""5 3 4 6 7 8 9 1 2
6 7 2 1 9 5 3 4 8
1 9 8 3 4 2 5 6 7
8 5 9 7 6 1 4 2 3
4 2 6 8 5 3 7 9 1
7 1 3 9 2 4 8 5 6
9 6 1 5 3 7 2 8 4
2 8 7 4 1 9 6 3 5
3 4 5 2 8 6 1 7 9""")

//   val matrix1 = matrix("""1 2 3 4 5 6 7 8 9
// 1 2 3 4 5 6 7 8 9
// 1 2 3 4 5 6 7 8 9
// 1 2 3 4 5 6 7 8 9
// 1 2 3 4 5 6 7 8 9
// 1 2 3 4 5 6 7 8 9
// 1 2 3 4 5 6 7 8 9
// 1 2 3 4 5 6 7 8 9
// 1 2 3 4 5 6 7 8 9""")

    def expected(n:Int) = (1 to n*n).toSet
    val expected_3 = expected(3)

  "it" should {
    " extract a square game " in {
      val result = squareGame(0, 0, 3, matrix1)

      result must_== expected_3
    }

    " extract sqaure games" in {
      val result = squareGames(3, matrix1)

      result.foreach( _ must_== expected_3 )
      result must have size(9)
    }

    " extract vertial games" in {
      val result = verticalGames(3, matrix1)

      result.foreach( _ must_== expected_3 )
      result must have size(9)
    }

    " extract horizontal games" in {
      val result = horizontalGames(3, matrix1)

      result.foreach( _ must_== expected_3 )
      result must have size(9)
    }

    " extract all games " in {
      val result = games(3, matrix1)

      result.foreach( _ must_== expected_3 )
      result must have size(3*3*3)
    }
  }

  "It" should {
    "run sample data" in {
      process(io.Source.fromFile("sample.in").getLines)(println)
      ok
    }
  }
}
