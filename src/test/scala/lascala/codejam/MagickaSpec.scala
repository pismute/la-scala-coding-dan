package lascala.codejam

import org.specs2.mutable._

object MagickaSpec extends Specification {

  import Magicka._

  "it" should {
    " pass #1" in {
      solve(Nil, Nil, "EA") must_==("[E, A]")
      solve(List("QRI"), Nil, "RRQR") must_==("[R, I, R]")
      solve(List("QFT"), List("QF"), "FAQFDFQ") must_==("[F, D, T]")
      solve(List("EEZ"), List("QE"), "QEEEERA") must_==("[Z, E, R, A]")
      solve(Nil, List("QW"), "QW") must_==("[]")
    }
    " pass #2" in {
      solve(List("QFT"), List("RF"), "QF") must_==("[T]")
      solve(List("QFT"), List("RF"), "QEF") must_==("[Q, E, F]")
      solve(List("QFT"), List("RF"), "RFE") must_==("[E]")
      solve(List("QFT"), List("RF"), "REF") must_==("[]")
      solve(List("QFT"), List("RF"), "RQF") must_==("[R, T]")
      solve(List("QFT"), List("RF"), "RFQ") must_==("[Q]")
    }
    " pass #3" in {
      solve(List("FRG"), List("QD"), "SQAERRFDRF") must_==("[G]")
    }
  }

}

/*

 http://etorreborre.github.io/specs2/guide/org.specs2.guide.Matchers.html

 ## Matcher

 ### Equality

 * 1
 * 1
 * toTrue
 * toFalse

 ### Any

 * beNull
 * beAsNullAs: when 2 objects must be null at the same time if one of them is null
 * [yas] elisp error: Symbol's value as variable is void: beLike: to check if an object is like a given pattern
 * beOneOf(a, b, c): to check if an object is one of a given list

 ### Option/Either/Try

 * beNone
 * beAsNoneAs
 * beSome(exp):
 * beSome.which(function)
 * beSome.like(partial function)

 * beRight(exp)
 * beRight.like(partial function)
 * beLeft.like(partial function)

 * beSuccessfulTry: checks if an element is Success(_)
 * beSuccessfulTry.withValue(exp) checks if an element is Success(_)
 * beFailedTry
 * beFailedTry.withThrowable[T]

 ### String

 * beMatching (or be matching): checks if a string matches a regular expression
 * =~(s): is a shortcut for beMatching("(.|\s)*"+s+"(.|\s)*")
 * find(exp).withGroups(a, b, c): checks if some groups are found in a string
 * have size(10): checks the size of a string (seen as an Iterable[Char])
 * be empty: checks if a string is empty
 * beEqualTo(b).ignoreSpace.ignoreCase.trimmed
 * contain(b)
 * startWith(b)
 * endWith(b)

 ### Numeric

 * 1 must be_<=(2)
 * 1 must be_<(2)
 * 2 must be_>=(1)
 * 2 must be_>(1)
 * 5 must beBetween(3, 6)

 ### Exception

 * throwA[ExceptionType](message = "boom")
 * throwA[ExceptionType].like { case e => e must matchSomething }
 * throwA(exception).like { case e => e must matchSomething }

 ### Traversable

 Seq() must be empty
 Seq(1, 2, 3) must not be empty
 Seq(1, 2) must have size(2)
 Seq(1, 2, 3) must beSorted
 Seq(1, 2, 3) must contain(2)
 Seq(1, 2, 3) must contain(be_>=(2))

 Seq(1234, 6237) must containMatch("23") // matches with ".*23.*"
 Seq(1234, 6234) must containPattern(".*234") // matches with !.*234"

 Seq(1, 2, 3) must contain(be_>(0)).forall // this will stop after the first failure
 Seq(1, 2, 3) must contain(be_>(0)).foreach // this will report all failures
 Seq(1, 2, 3) must contain(be_>(0)).atLeastOnce
 Seq(1, 2, 3) must contain(be_>(2)).atMostOnce
 Seq(1, 2, 3) must contain(be_>(2)).exactly(1.times)
 Seq(1, 2, 3) must contain(be_>(2)).exactly(1)
 Seq(1, 2, 3) must contain(be_>(1)).between(1.times, 2.times)
 Seq(1, 2, 3) must contain(be_>(1)).between(1, 2)

 Seq(1, 2, 3, 4) must contain(2, 4)
 Seq(1, 2, 3, 4) must contain(allOf(2, 4))
 Seq(1, 2, 3, 4) must contain(allOf(be_>(0), be_>(1)))
 Seq(1, 2, 3, 4) must contain(allOf(be_>(0), be_>(1)).inOrder)
 Seq(1) must contain(allOf(1, 1)).onDistinctValues

 Seq(1, 2, 3, 4) must contain(atLeast(2, 4))
 Seq(2, 3) must contain(atMost(2, 3, 4))
 Seq(1, 2) must contain(exactly(2, 1))

 Seq(2, 4, 1) must containTheSameElementsAs(Seq(1, 4, 2))

 ### Map

 Map(1 -> "1") must haveKey(1)
 Map(1 -> "1", 2 -> "2") must haveKeys(1, 2)
 Map(1 -> "1", 2 -> "2") must haveValue("1", "2")
 Map(1 -> "1") must havePair(1 -> "1")
 Map(1->"1", 2->"2", 3->"3") must havePairs(1->"1", 2->"2")

 */
