class person(val name: String, var age: Int){
  def getName = name
  def getAge = age
  override def toString() = getName + ", " + getAge
}

object person {
  def apply(name: String, age: Int): person ={
    new person(name, age)
  }
  def unapply(input: person) = Some((input.getName, input.getAge))
}

class fraction(val numerator: Int, val denominator: Int){
  def getDecimal(): Double = {
    (numerator*1.0) / (denominator*1.0)
  }
  override def toString() = numerator.toString + "/" + denominator.toString + " | Dec: " + getDecimal().toString
}

object fraction{
  def apply(numerator: Int, denominator: Int): fraction ={
    new fraction(numerator, denominator)
  }
  def unapply(input: fraction) = Some((input.numerator, input.denominator))
}

object extractorTestor {
  def main(args:Array[String]) = {
    var person1 = person("Nate", 20)
    var fractio = fraction(4,7)

    person1 match{
      case person("Dr. Hu", 45) => println("Welcome, doctor!")
      case person("Nate", 20) => println("Oh...it's you again.")
    }

    fractio match {
      case fraction(1,2) => println(0.5)
      case fraction(3,4) => println(0.75)
      case fraction(4,7) => println(fraction(4,7).toString)
    }
  }
}