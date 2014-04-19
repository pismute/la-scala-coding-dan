package lascala.codejam.twelve.dancing_with_the_googlers

import org.specs2.mutable._

class DancingWithTheGooglersSpec extends Specification {

  implicit class DancingList[A](list: List[A]) {
    def combinationsWithRepetition(n: Int): Iterator[List[A]] = {
      list.flatMap( List.fill(n)(_) ).combinations(n)
    }
  }

  def happen(abc:List[Int]) = {
    abc.max - abc.min <= 2
  }

  def isSurprising(abc:List[Int]) = {
    abc.max - abc.min == 2
  }

  def cartesianProduct(src: List[List[_]]) = {
  }

  "DancingWithTheHelpers" should{
    "make combinations With Repetition" in {
      val triplet = List(5, 4, 6)

      val expected = List(
        List(5, 5, 5), List(5, 5, 4), List(5, 5, 6),
        List(5, 4, 4), List(5, 4, 6), List(5, 6, 6),
        List(4, 4, 4), List(4, 4, 6), List(4, 6, 6),
        List(6, 6, 6)
      )

      triplet.combinationsWithRepetition(3).toList must_==(expected)
    }

    "is Surprising" in {
      isSurprising(List(5, 4, 6)) must_==(true)
    }
  }

  "DancingWithTheGooglers" should{
    "'3 1 5 15 13 11'" in {
      val nOfGooglers = 3
      val nOfSurprising = 1
      val p = 5
      val scores = List(15, 13, 11)
      val nOfJudges = 3

      val allCombinations: List[List[List[_]]] = scores.map { (score:Int)=>
        val mean:Int = score/nOfJudges

        //mean deviation is greater than 2 never happens.
        List(mean -1, mean, mean + 1, mean + 2)
        .combinationsWithRepetition(nOfJudges).toList
          .filter( _.sum == score )
          .filter( _.max >= p ) //We need only the number of 'max >= p'
          .filter( happen )
          //.map( (triplet) => (isSurprising(triplet), triplet) )
      }.sortBy(_.size) //for

      allCombinations.foreach(println)

      ok
    }
  }
}
