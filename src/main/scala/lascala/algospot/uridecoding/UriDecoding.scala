package la.scala.algospot.uridecoding

import scala.util.matching.Regex.Match

object Main {
  def main(args: Array[String]): Unit = {
    val cases = Integer.parseInt(readLine())
    for( i <- 0 until cases){
      println( decode( readLine() ) )
    }
  }

  val hexPattern = "%(.{2})".r

  def decodeChar(s:String):Option[String] =
    //after 2.10.x
    //Try( Integer.parseInt(s, 16) ).map(_.toChar).toOption
    try {
      Some( Integer.parseInt(s, 16).toChar.toString )
    } catch {
      case e:Exception => None
    }

  def decode(s:String):String = {
    hexPattern.replaceSomeIn(s, (m:Match)=> decodeChar( (m group 1) ) )
  }
}
