object Exercise3_FactorialOP extends App{

  class RichInt(val value: Int) {
    def !():Int = (1 to value).foldLeft(1)( (res, v) => res * v )
    override def toString: String = value.toString
  }

  implicit def int2RichInt(value: Int): RichInt = new RichInt(value)

  var myInt = new RichInt(3)
  var myIntFact = (myInt!)
  println(myInt + "! = " + myIntFact)
}
