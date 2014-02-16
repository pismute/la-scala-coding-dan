package la.scala.algospot.wierd

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import scala.util.matching.Regex.Match

class WierdSpec extends FunSpec with ShouldMatchers{
  import Main._

  it("getProperDivisors() should generate proper divisors from n.") {
    getProperDivisors(12).sorted should be (List(1,2,3,4,6))
    getProperDivisors(4).sorted should be (List(1,2))
    getProperDivisors(70).sorted should be (List(1, 2, 5, 7, 10, 14, 35))
  }

  it("Sum of its proper divisors (i.e. less than N ) is greater than the number.") {
    isSumGreaterThan( List(1, 2, 3, 4, 6), 12) should be (true)
    isSumGreaterThan( List(1, 2, 5, 7, 10, 14, 35), 70) should be (true)
  }

  it("is it wierd?"){
    isWierd(12) should be (false)
    isWierd(70) should be (true)
  }
}
