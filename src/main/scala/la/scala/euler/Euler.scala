package la.scala.euler

import scala.math
import scala.annotation.tailrec

object Euler {
  def isPrime(n:Long) : Boolean = {
    require( n > 0 , "Negative number")

    n match {
      case 0 | 1 => false
      case 2 => true
      case _ if n % 2 == 0 => false;
      case _ =>
        ! (3L to math.sqrt(n).toLong ).exists(n % _ == 0)
    }
  }

  def getSmallestPrime(n:Long):Long = {
    require( n > 0 , "Negative number")

    n match {
      case 0L | 1L => 0L
      case 2L => 2L
      case p if p % 2L == 0L => 2L
      case _ =>
        (3L to math.sqrt(n).toLong ).find(n % _ == 0L ) match {
          case Some(nb) => nb
          case None => n
        }
    }
  }

  @tailrec
  def getLargestPrime(n:Long):Long = Euler.getSmallestPrime(n) match{
    case 0 => n
    case p if p >= n => n
    case p => getLargestPrime( n / p )
  }

  /**
   * ## long range
   *
   * - [Iterator](https://blogs.warwick.ac.uk/chrismay/entry/long_ranges_in)
   * - [Stream](https://gist.github.com/daclouds/67b184fdb1b1a36bd181)
   * - Recursion
   */
  @tailrec
  def nthPrime(nth:Long, i:Long=1, count:Long = 0):Long = (i, count) match {
    case (p, c) if c == nth => p-1
    case (p, _) if Euler.isPrime(p) => nthPrime(nth:Long, i+1, count+1)
    case _ => nthPrime(nth, i+1, count)
  }

  @tailrec
  def gcd(l:Long, r:Long):Long = r match {
    case 0 => l
    case _ => gcd(r, l % r)
  }

  def lcm(l:Long, r:Long):Long = l / gcd(l, r) * r

  def isPalindrome(n:String):Boolean = n.reverse.equals( n )
}
