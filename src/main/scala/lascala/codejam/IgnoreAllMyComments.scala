package lascala.codejam

/*
 * problem links
 */
object IgnoreAllMyComments {
  type Next = Function[Char, Any]

  def `/`(c:Long)(implicit put: (Char) => Unit):Next = {
    case '/' if c == 0 => `/`(c)
    case '/' => `/`(c)
    case '*' => `_`(c+1)
    case x if c == 0 =>
      put('/')
      put(x)
      `_`(c)
    case x => `_`(c)
  }

  def `*`(c:Long)(implicit put: (Char) => Unit):Next = {
    case '/' => `_`(if(c > 0) c-1 else 0)
    case '*' => `*`(c)
    case x => `_`(c)
  }

  def `_`(c:Long)(implicit put: (Char) => Unit):Next = {
    case '/' => `/`(c)
    case '*' if c > 0 => `*`(c)
    case x if c == 0=>
      put(x)
      `_`(c)
    case x => `_`(c)
  }

  def process(source: Iterator[Char])(implicit put:(Char)=>Unit) = {
    "Case #1:\n".foreach(put)
    (`_`(0) /: source){ (next, c) =>
      next(c).asInstanceOf[Next]
    }
  }

  def main(args: Array[String]):Unit = {
    val writer = new java.io.PrintWriter("a.out")

    implicit val put:(Char)=>Unit = writer.print

    try {
      //process(io.Source.fromFile("E-small-practice.in").iter)
      process(io.Source.fromFile("E-large-practice.in").iter)
      //process(io.Source.fromFile("example.in").iter)(print)
    } finally {
      writer.flush(); writer.close()
    }
  }
}
