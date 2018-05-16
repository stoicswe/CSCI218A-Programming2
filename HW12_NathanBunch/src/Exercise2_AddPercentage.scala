object Exercise2_AddPercentage extends App {

  class RichInt(val value: Int) {
    def +%(percent: Double): Double = value + (value * (1/percent))
    override def toString: String = value.toString
  }

  implicit def intAddPercent(value: Int): RichInt = new RichInt(value)

  var myInt = new RichInt(50)
  println(myInt.toString)
  var newDouble = myInt +% 10
  println(newDouble)
}