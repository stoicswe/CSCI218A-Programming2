object convertDefinitionsTest extends App {
  var myList = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
  var myArray = Array(1,2,3,4,5)
  var mySeq = Seq(1,2,3,4,5)
  var myDefs = new convertDefinitions
  println("Sums:")
  println(myDefs.sumList(myList))
  println(myDefs.sumList2(myList, 0))
  println(myDefs.sumArray2(myArray))
  println(myDefs.sumArray2_tailRecursive(myArray, 0))
  println(myDefs.sumRecFun(myList))
  println("Factorial (7):")
  println(myDefs.factorial(7))
  println()
  println("Fold left, fold right:")
  println(myDefs.myfoldLeft(mySeq, 0)(_+_))
  println()
  println(myDefs.myfoldRight(mySeq, 0)(_+_))
  println()
  println("GCD:")
  println(myDefs.gcd(7,9))
  println(myDefs.gcd2(7,9))
}
