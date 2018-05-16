object Exercise5_SmallerFraction extends App {

  case class Fraction(val numerator: Int, val denominator: Int){
    override def toString: String = numerator + "/" + denominator
  }

  def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]): T = if (a < b) a else b

  class RichFraction(val fraction: Fraction) extends Ordered[Fraction] {
    //this minus that
    def compare(that: Fraction):Int = (fraction.numerator * that.denominator) - (that.numerator * fraction.denominator)
  }

  implicit def fraction2RichFraction(f: Fraction): RichFraction = new RichFraction(f)

  var fract1 = new Fraction(4,5)
  var fract2 = new Fraction(2,3)
  var comparedValue = smaller(fract1, fract2)
  println(fract1 + " < " + fract2 + " = " + comparedValue)
}
