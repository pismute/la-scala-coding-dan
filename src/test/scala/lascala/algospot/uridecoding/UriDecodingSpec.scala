package la.scala.algospot.uridecoding

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import scala.util.matching.Regex.Match

class UrlDecodingSpec extends FunSpec with ShouldMatchers{
  import Main._

  describe("decodeChar") {
    describe("should") {
      it("decode hexcode to char") {
        decodeChar("2a").get should be ("*")
        decodeChar("25").get should be ("%")
      }
    }
  }

  describe("decode") {
    describe("should") {
      it("decode url") {
        decode( "Happy%20Joy%20Joy%21") should be ("Happy Joy Joy!")
        decode( "http://algospot.com/%2a") should be ("http://algospot.com/*")
        decode("%ka") should be ("%ka")
      }
    }
  }
}
