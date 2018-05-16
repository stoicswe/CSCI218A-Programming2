package factorialApplication

object factorialApp extends App {

  def factRecursion(x: Int): Int = {
    if (x < 1)
      1
    else
      x * factRecursion(x-1)
  }
  def factRedLeft(x: Int): Int = {
    if (x < 1) 1 else (x to 1 by -1).reduceLeft(_ * _)
  }
  def factFor(x: Int): Int = {
    var l = 1
    if (x < 1)
      1
    else
      for (i <- x to 1 by -1)
        l = i * l
    l
  }
  def factWhile(x: Int): Int = {
    var y = x
    var l = 1
    if (y < 1)
      1
    else
      while(y > 1){
        l = l*y
        y -= 1
      }
    l
  }
  def factFold(x: Int): Int = {
    (1 to x).foldLeft(1)(_ * _)
  }
  def factMatch(x: Int): Int = {
    var num = 1
    x match {
      case 0 => 0
      case 1 => 1
      case _ if (x < 0) => { x * factMatch(x+1)}
      case _ => {x * factMatch(x-1)}
    }
  }

  println(factRecursion(7))
  println(factRedLeft(7))
  println(factFor(7))
  println(factWhile(7))
  println(factFold(7))
  println(factMatch(7))

}
