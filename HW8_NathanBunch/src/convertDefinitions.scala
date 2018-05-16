import scala.annotation.tailrec

class convertDefinitions {
  //convert to tail recursive
  def sumList(list: List[Int]): Int = list match {
    case Nil => 0
    case x :: xs => x + sumList(xs)
  }

  @tailrec final def sumList2(list: List[Int], partial: BigInt): BigInt =
    list match {
    case Nil => partial
    case x :: xs => sumList2(xs, x + partial)
  }
  //========================================================================================

  //convert for an array, than make it a tail recursive
  def sumArray(list: List[Int]): Int = list match {
    case Nil => 0
    case x :: xs => x + sumArray(xs)
  }

  def sumArray2(array: Array[Int]): Int =
    array match{
    case Array() => 0
    case Array(x, xs @ _*) => x + sumArray2(xs.toArray)
  }

  @tailrec final def sumArray2_tailRecursive(array: Array[Int], partial: Int): Int =
    array match{
      case Array() => partial
      case Array(x, xs @ _*) => sumArray2_tailRecursive(xs.toArray, partial + x)

  }
  //========================================================================================

  //create a tail recursive inside function
  def sumRecFun(list: List[Int]): Int = {
    @tailrec
    def sumAccumulator(lister: List[Int], accum: Int): Int = {
      lister match {
        case Nil => accum
        case x :: tail => sumAccumulator(tail, accum + x)
      }
    }
    sumAccumulator(list, 0)
  }


  //========================================================================================

  //do all the above for the product of the list / array

  //complete followiung by filing in ABC
  def factorial(n: Int): Int = {
    @tailrec
    def iter(x: Int, result: Int): Int =
      if (x == 0) result
      else iter(x - 1, result * x)

    iter(n, 1)
  }
  //========================================================================================

  //show that this is actually folding left by printing statements
  def myfoldLeft[A,B] (seq: Seq[A], z:B) (f: (B,A) => B) : B =
    seq match {
      case Nil => {println(z); z}
      case x :: xs => {
        println(x + " op " + xs + " -> " + z + " ")
        myfoldLeft(xs, f(z,x))(f)
      }
    }
  //========================================================================================

  //recreate above for a foldright method instead
  //and include print statements so that you can show it works
  /*
  def reverseSeqToString[T](l: Seq[T]): String = l match {
  case prefix :+ end => reverseSeqToString(prefix) + s" :+ $end"
  case Nil => "Nil"
}
   */
  @tailrec final def myfoldRight[A,B] (seq: Seq[A], z:B) (f: (B,A) => B) : B =
    seq match {
      case Nil => {println(z); z}
      case x :+ xs => {
        println(x + " op " + xs + " -> " + z + " ")
        myfoldRight(x, f(z,xs))(f)
      }
    }
  //========================================================================================

  //this is for gcd, recursively....recreate using for loops
  @tailrec
  final def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def gcd2(first: Int, second:Int): Int = {
    var fact = Math.max(first, second)
    for(i <- fact until 1){
      if (first % i == 0 && second % i == 0){
        return i
      }
    }
    1
  }
}
