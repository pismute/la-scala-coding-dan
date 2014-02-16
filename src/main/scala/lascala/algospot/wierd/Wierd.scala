package la.scala.algospot.wierd

import scala.util.matching.Regex.Match

object Main {
  def main(args: Array[String]): Unit = {
    val cases = Integer.parseInt(readLine())
    for( i <- 0 until cases){
      //println( decode( readLine() ) )
    }
  }

  def getProperDivisors(n:Int):List[Int] = {
    val root = math.sqrt(n).toInt

    var pds:List[Int] = Nil

    if(root*root == n ) {
      pds = (2 until root).filter(n%_==0).toList

      1 :: root :: pds ++ pds.map( n/_ )
    } else {
      pds = (2 to root).filter(n%_==0).toList

      1 :: pds ++ pds.map( n/_ )
    }

  }

  def isSumGreaterThan(l:List[Int], n:Int) = l.sum > n

  def isWierd(n:Int):Boolean = {
    val pds = this.getProperDivisors(n)

    isSumGreaterThan(pds, n) && !pds.toSet.subsets.exists( _.sum == n )
 }
}
