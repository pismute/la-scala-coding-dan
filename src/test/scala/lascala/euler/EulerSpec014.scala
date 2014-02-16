package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._
import scala.collection.mutable
 
class EulerSpec014 extends FunSpec with ShouldMatchers{
  ignore("should be 837799") {
    @tailrec
    def collatz(n:BigInt, chain:Long = 1L):Long = n match {
      case _ if n == 1 => chain
      case _ if n % 2 == 0 => collatz(n/2, chain + 1)
      case _ => collatz( (3 * n) + 1, chain + 1)
    }

    Stream.from(1).take(1000000)
      .foldLeft( (0, 0) )( (max, n) => collatz(n) match {
        case c if max._1 < c => (c.toInt, n)
        case _ => max
      })
      ._2 should be (837799)
  }

  //Heuristic
  ignore("should be 837799 in heuristic") {
    val vals:mutable.Map[BigInt, Long] = mutable.Map.empty

    @tailrec    
    def collatz(n:BigInt, chain:List[BigInt] = Nil):Long = n match {
      case _ if n == 1 =>
        chain.zipWithIndex.foreach{ case (el, i) => vals.put(el, i + 2) }
        chain.size + 1
      case _ if vals.contains(n) =>
        val steps = vals(n)
        chain.zipWithIndex.foreach{ case (el, i) => vals.put(el, i + steps) }
        chain.size + steps
      case _ if n % 2 == 0 => collatz(n/2, n :: chain )
      case _ => collatz( (3 * n) + 1, n :: chain )
    }

    Stream.from(1).take(1000000)
      .foldLeft( (0, 0) )( (max, n) => collatz(n) match {
        case c if max._1 < c => (c.toInt, n)
        case _ => max
      })
      ._2 should be (837799)
  }

  // Memoization
  // from http://michid.wordpress.com/2009/02/23/function_mem/ 
  ignore("should be 837799, accessible recursive from outside") {
    def collatzRec(n:BigInt, chain:Long, f: (BigInt, Long)=>Long):Long  = n match{
      case _ if n == 1 => chain
      case _ if n % 2 == 0 => f(n/2, chain + 1)
      case _ => f( (3 * n) + 1, chain + 1)
    }

    var collatz: (BigInt, Long)=>Long = null
    collatz = collatzRec(_, _, collatz(_, _))

    Stream.from(1).take(1000000)
      .foldLeft( (0, 0) )( (max, n) => collatz(n, 1L) match {
        case c if max._1 < c => (c.toInt, n)
        case _ => max
      })
      ._2 should be (837799)
  }

  // Memoization
  // from http://michid.wordpress.com/2009/02/23/function_mem/ 
  it("should be 837799 in memoization") {
    class Memoize1[-K, -A, +R](f: (K, A) => R) extends ( (K, A) => R) {
      import scala.collection.mutable
      private[this] val vals = mutable.Map.empty[K, R]

      def apply(x: K, a:A): R = {
        if (vals.contains(x)) {
          vals(x)
        }
        else {
          val y = f(x, a)
          vals + ((x, y))
          y
        }
      }
    }

    object Memoize1 {
      def apply[K, A, R](f: (K, A) => R) = new Memoize1(f)

      def Y[K, A, R](f: (K, A, (K, A) => R) => R) = {
        var yf: (K, A) => R = null
        yf = Memoize1(f(_, _, yf(_, _)))
        yf
      }
    }

    def collatzRec(n:BigInt, chain:Long, f: (BigInt, Long)=>Long):Long  = n match{
      case _ if n == 1 => chain
      case _ if n % 2 == 0 => f(n/2, chain + 1)
      case _ => f( (3 * n) + 1, chain + 1)
    }

    val collatz = Memoize1.Y(collatzRec)

    Stream.from(1).take(1000000)
      .foldLeft( (0, 0) )( (max, n) => collatz(n, 1L) match {
        case c if max._1 < c => (c.toInt, n)
        case _ => max
      })
      ._2 should be (837799)
  }
}
