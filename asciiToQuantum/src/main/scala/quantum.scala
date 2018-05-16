import scala.io.StdIn

object quantum extends App {
  def convertToQuantum(phrase: String): String ={
    var bits = phrase.toCharArray
    var quantumCode = ""

    for (i <- 0 until phrase.length-2 by 2){
      println(phrase(i) + " " + phrase(i+1))
      if(phrase(i).equals('0') && phrase(i+1).equals('0')){
        quantumCode += "2^-0.5 (|00> + |11>) "
      }

      if(phrase(i).equals('0') && phrase(i+1).equals('1')){
        quantumCode += "2^-0.5 (|10> + |01>) "
      }

      if(phrase(i).equals('1') && phrase(i+1).equals('0')){
        quantumCode += "2^-0.5 (|00> - |11>) "
      }

      if(phrase(i).equals('1') && phrase(i+1).equals('1')){
        quantumCode += "2^-0.5 (-|10> + |01>) "
      }
    }

    return quantumCode
  }

  while(true){
    print("Enter a phrase: ")
    var input = StdIn.readLine()
    println(convertToQuantum(input))
  }
}
