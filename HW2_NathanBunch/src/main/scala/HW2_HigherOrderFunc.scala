import scala.collection.mutable.ArrayBuffer

object HW2_HigherOrderFunc extends App {

  def applyFunct(funct: (Double) => Double, num: Double, times: Int): Double ={
    var calculatedDouble = 0.0
    for(i <- 0 to times) calculatedDouble = funct(calculatedDouble)
    calculatedDouble
  }

  def mapping(funct: (Int) => Int, upperBound: Int): ArrayBuffer[Int] = {
    var numbers = ArrayBuffer[Int]()
    for (i <- 1 to upperBound) {
      numbers += funct(i)
    }
    numbers
  }

  def manipulate(funct: (Char) => String, stringToMod: String): String ={
    var newString = ""
    for(i <- 0 to stringToMod.length - 1){
      newString += funct(stringToMod(i))
    }
    newString
  }

  //start returning functions

  def genMultFunct(factor: Double): (Double) => Double ={
    (X: Double) => (factor * X)
  }

  def genStrGrabFunct(place: Int): (String) => String = {
    (str: String) => ({ var returnStr = ""; for (i <- 0 to place){ returnStr += str(i) }; returnStr; })
  }

  def reductionFunct(functToApply: (Int, Int) => Int): (Int, Int) => Double ={
    return (lower: Int, upper: Int) => {
      (lower to upper).reduceLeft(functToApply)
    }
  }

  //====================================================================================================
  //above this line are the functions for the homework.
  //the contents beneath this line is for testing the above functions.

  def testFunction(input: Double): Double = {
    math.sqrt(2.5*input + math.pow((math.Pi), input))
  }

  def testFunction1(input: Int): Int ={
    ((math.Pi + math.E) * input).toInt
  }

  def testFunction2(input: Char): String ={
    input.toByte.toString
  }

  var myFunct = genMultFunct(5.6)
  var strGrabber = genStrGrabFunct(3)
  var reduct = reductionFunct((X: Int, Y: Int) => (X%Y))

  println(applyFunct(testFunction, 5.7, 2))
  println(mapping(testFunction1, 45))
  println(manipulate(testFunction2, "Hello World!"))
  println(myFunct(34.6))
  println(strGrabber("Super Powers of Scala"))
  println(reduct(1, 9))

}
