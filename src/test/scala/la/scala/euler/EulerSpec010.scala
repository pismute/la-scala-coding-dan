package la.scala.euler

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import util.control.Breaks._

class EulerSpec010 extends FlatSpec with ShouldMatchers{
  "euler001" should "be 233168" in {
    @tailrec
    def euler001(i:Int, accu:Int = 0):Int = i match{
      case 1000 => accu
      case x if ( x % 3 == 0 || i % 5 == 0 ) => euler001(i + 1, accu + i)
      case _ => euler001(i + 1, accu)
    }

    euler001(0) should be (233168)
  }
  
  "euler002 " should "be 4613732" in {
    @tailrec
    def finonacci(a:Int, b:Int, accu:Int = 0):Int = (a, b) match {
      case (x, y) if ( y > 4000000 ) => accu 
      case (x, y) if ( y % 2 == 0 ) => finonacci(y, x + y, accu + y)
      case (x, y) => finonacci(y, x + y, accu)
    }
    
    finonacci(1, 2) should be (4613732)
  }
  
  "euler003 " should "be 6857" in {
    Euler.getLargestPrime(600851475143L) should be (6857)
  }
  
  "euler004 " should "be 906609" in {
    (for {
      i <- 100 until 1000
      j <- 100 until 1000
      k = i * j   
      if Euler.isPalindrome( k.toString )
    } yield k).max should be (906609)
  }
  
  "euler005 " should "be 232792560" in {
    (2L to 20L).foldLeft(1L)( Euler.lcm(_, _) ) should be (232792560)
  }

  "euler006 " should "be 25164150" in {
    val natural = 1L to 100L
    val sumOfSquare = natural.foldLeft(0L)( (sum, n) => sum + n * n)
    val squareOfSum = natural.sum * natural.sum
    
    (squareOfSum - sumOfSquare) should be (25164150)
  }

  "euler007 " should "be 104743" in {
    Euler.nthPrime(10001) should be (104743)
  }

  "euler008 " should "be 40824" in {
    def i(c:Char):Int = c.toInt - 48
    
    def Numbre = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    
    @tailrec
    def getConsecutiveDigits(n:String, accu:List[List[Int]] = Nil):List[List[Int]] =  n.length match {
      case l if l < 5 => accu
      case _ => getConsecutiveDigits (n.tail, 
        (i( n.head ) ::
          i( n.tail.head ) ::
          i( n.tail.tail.head ) ::
          i( n.tail.tail.tail.head ) ::
          i( n.tail.tail.tail.tail.head ) :: 
          Nil) :: accu)
    }
    
    getConsecutiveDigits(Numbre).map(_.product).max should be (40824)
  }
    
  "euler009 " should "be 31875000" in {
    @tailrec
    def euler0091(a:Long, b:Long):Long = (a, b, 1000L-a-b) match {
      case (a, b, c) if c > b && a * a + b * b == c * c => a*b*c
      case (_, b, _) if b >= 1000 => 0
      case _ => euler0091( a, b+1 )
    }

    @tailrec
    def euler009(a:Long):Long = euler0091(a, a+1) match {
      case 0 => euler009(a+1)
      case x => x
    }
    
    euler009(1) should be (31875000)
  }
  
  "euler010 " should "be 142913828922" in {
    (1L to 2000000L)
      .filter( Euler.isPrime )
      .sum should be (142913828922L)
  }
}