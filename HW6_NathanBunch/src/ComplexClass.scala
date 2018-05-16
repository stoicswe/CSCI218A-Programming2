class ComplexNumber(val complx: (Double, Double)){
  def +(other: ComplexNumber): ComplexNumber = {
    println("(" + this.toString + ") + (" + other.toString + ")")
    ComplexNumber(this.complx._1 + other.complx._1, this.complx._2 + other.complx._2)
  }
  def -(other: ComplexNumber): ComplexNumber = {
    println("(" + this.toString + ") - (" + other.toString + ")")
    ComplexNumber(this.complx._1 - other.complx._1, this.complx._2 - other.complx._2)
  }
  def *(other: ComplexNumber): ComplexNumber = {
    println("(" + this.toString + ") * (" + other.toString + ")")
    ComplexNumber(this.complx._1 * other.complx._1 - this.complx._2 * other.complx._2, this.complx._1 * other.complx._2 + this.complx._2 * other.complx._1)
  }
  def +(real: Double): ComplexNumber = {
    ComplexNumber(this.complx._1+real, this.complx._2)
  }
  def -(real: Double): ComplexNumber = {
    ComplexNumber(this.complx._1 - real, this.complx._2)
  }
  def normalize(comp: ComplexNumber): Double = {
    Math.sqrt(comp.complx._1 * comp.complx._1 + comp.complx._2 * comp.complx._2)
  }
  def normalize(): Double = {
    Math.sqrt(this.complx._1 * this.complx._1 + this.complx._2 * this.complx._2)
  }
  override def toString: String = complx._1.toString + " + " + complx._2.toString + "i"
}

object ComplexNumber {
  def apply(complex: (Double, Double)): ComplexNumber ={ new ComplexNumber(complex);}
  def apply(realNum: Double): ComplexNumber = {new ComplexNumber(realNum, 0.0)}
  def unapply(complexNumber: ComplexNumber) = complexNumber.complx
}

object ComplexClass extends App {
  var myComplex0 = ComplexNumber(5,7)
  var myComplex1 = ComplexNumber(3,2)
  var myComplex2 = ComplexNumber(4)

  println(myComplex0)
  println(myComplex1)
  println(myComplex2)
  println()
  println(myComplex0.normalize())
  println(myComplex1.normalize())
  println(myComplex2.normalize())
  println()
  println(myComplex0 + myComplex1)
  println(myComplex0 - myComplex1)
  println(myComplex0 * myComplex1)
  println()
  println(myComplex1 + myComplex2)
  println(myComplex1 - myComplex2)
  println(myComplex1 * myComplex2)
  println()
  println(myComplex0 + 3)
  println(myComplex0 - 3)
}
