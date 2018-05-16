import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.{Source}

object CSV_Manipulator extends App {

  //define useful functions

  def readCSV(path: String): Array[String] ={
    var csvData = new ListBuffer[Array[String]]
    var chars = Source.fromFile(path).getLines().toArray
    return chars
  }

  def printDF(frame: ListBuffer[Array[String]]) {
    for (row <- frame){
      printRow(row)
    }
    println("\n\n\n")
  }

  def printRow(currentRow: Array[String]) {
    for (value <- currentRow){
      print(" | " + value + " | ")
    }
    print("\n")
  }

  def search(item: String, dfrow: Array[String]): Int ={
    for (i <- 0 to dfrow.length - 1){
      if (dfrow(i).equals(item))
        return i
    }
    -1
  }

  def getColumn(colIndex: Int, df: ListBuffer[Array[String]]): ArrayBuffer[String] ={
    var returnRow = ArrayBuffer[String]()
    for(row <- df){
      if(row.length > colIndex)
        returnRow += row(colIndex)
    }
    returnRow
  }

  def getRows(colIndex: Int, df: ListBuffer[Array[String]], condition: String): ListBuffer[Array[String]] ={
    var returnDF = ListBuffer[Array[String]]()
    for(row <- df){
      if(checkValue(condition, row)){
        returnDF += row
      }
    }
    returnDF
  }

  def getRow(rowIndex: Int, df: ListBuffer[Array[String]]): Array[String] ={
    df(rowIndex)
  }

  def clearDuplicates(row: Array[String]): ArrayBuffer[String] ={
    var cleanedRow = ArrayBuffer[String]()
    for(value <- row){
      if (!checkValue(value, cleanedRow.toArray)){
        cleanedRow += value
      }
    }
    cleanedRow
  }

  def checkValue(value: String, row: Array[String]): Boolean ={
    row contains value
  }

  def sumColumn(colIndex: Int, startRow: Int, df: ListBuffer[Array[String]]): Double ={
    var sum = 0.0
    try{
      for(i <- startRow to df.length-1){
        var row = df(i)
        sum += row(colIndex).toDouble
      }
      sum
    } catch {
      case e => println("Error with Sum."); 0
    }
  }

  def sumColumn(colIndex: Int, startRow: Int, df: ListBuffer[Array[String]], condition: String): Double ={
    var sum = 0.0
    try{
      for(i <- startRow to df.length-1){
        var row = df(i)
        if(checkValue(condition, row))
          sum += row(colIndex).toDouble
      }
      sum
    } catch {
      case e => println("Error with Sum."); 0
    }
  }

  def count(df: ListBuffer[Array[String]]): Int ={
    var counter = 0
    for(row <- df){
      counter+=1
    }
    counter
  }

  def count(row: Array[String]): Int ={
    var counter = 0
    for(value <- row){
      counter+=1
    }
    counter
  }

  def pull(df: ListBuffer[Array[String]], expression: String, startRow: Int): ListBuffer[Array[String]] ={
    var pullValues = expression.split(" ")
    var returnDF = ListBuffer[Array[String]]()
    var colIndex = search(pullValues(0), df.head)

    for(i <- startRow to df.length-1){
      try {
        var row = df(i)
        if (pullValues(1).equals("<")) {
          if (row(colIndex).toInt < pullValues(2).toInt) {
            returnDF += row
          }
        }

        if (pullValues(1).equals(">")) {
          if (row(colIndex).toInt > pullValues(2).toInt) {
            returnDF += row
          }
        }

        if (pullValues(1).equals("==")) {
          if (row(colIndex).equals(pullValues(2))) {
            returnDF += row
          }
        }
      } catch {
        case e => ()
      }
    }
    returnDF
  }//this is a work in progress, does the basic function required

  def pull(colIndex: Int, df: ListBuffer[Array[String]], condition: String): Array[String] ={
    var returnRow = df(0)
    for(i <- 0 to count(df) - 1){
      var currentRow = df(i)
      for (j <- 0 to count(df) - 1){
        if (condition.equals("MAX")){
          if (returnRow(colIndex).toDouble < currentRow(colIndex).toDouble){
            returnRow = currentRow
          }
        }

        if (condition.equals("MIN")){
          if (returnRow(colIndex).toDouble > currentRow(colIndex).toDouble){
            returnRow = currentRow
          }
        }
      }
    }
    returnRow
  }

  def sort(colIndex: Int, startRow: Int, df: ListBuffer[Array[String]], condition: String): ListBuffer[Array[String]] ={
    var swapRow = Array[String]()
    var counter = 0
    if (condition.equals("DESC")){
      while(counter < count(df)*20){
        for(i <- startRow to df.size - 3){
          var currentRow = df(i)
          /*if (i != df.size - 3) {
            if (df(i + 1)(colIndex).toInt >= currentRow(colIndex).toInt) {}
          }*/

          if (i != startRow) {
            if (df(i - 1)(colIndex).toInt <= currentRow(colIndex).toInt) {
              df(i) = df(i - 1)
              df(i - 1) = currentRow
            }
          }
          counter+=1
        }
      }
    }
    return df
  }//work in progress, but works for what I need it to

  def sort(colIndex: Int, df:ListBuffer[Array[String]], condition:String): ListBuffer[Array[String]] ={
    var swapRow = Array[String]()
    var counter = 0
    if (condition.equals("DESC")){
      while(counter < count(df)*20){
        for(i <- 1 to df.size - 1){
          var currentRow = df(i)
          /*if (i != df.size - 3) {
            if (df(i + 1)(colIndex).toInt >= currentRow(colIndex).toInt) {}
          }*/

          if (df(i - 1)(colIndex).toInt <= currentRow(colIndex).toInt) {
            df(i) = df(i - 1)
            df(i - 1) = currentRow
          }
          counter+=1
        }
      }
    }
    return df
  }

  def combine(df: ListBuffer[Array[String]], otherDF: ListBuffer[Array[String]]): ListBuffer[Array[String]] ={
    for(row <- otherDF){
      df += row
    }
    df
  }

  //================================================================================
  //start the program / analysis


  var studentData = ListBuffer.empty[Array[String]]
  var examData = ListBuffer.empty[Array[String]]

  val whereami = System.getProperty("user.dir")
  var students = readCSV(whereami + "/Students.csv") //read CSV files
  var exams = readCSV(whereami + "/Exams.csv")

  //var tempString = ""
  //var tempChar = "" //thought this was necessary, but really wasnt
  //var tempBufferArray = ArrayBuffer[String]()

  for (line <- students){
    studentData += line.toString.split(",")(1) //setup for analysis
  }

  for (line <- exams){
    examData += line.toString.split(",")(1) + "," + line.toString.split(",")(2) //setup for analysis
  }

  //println(studentData)
  //println(examData)
  printDF(studentData) //print data
  printDF(examData)

  val majorColumn = clearDuplicates(getColumn(search("Major", studentData.head), studentData).toArray)
  printRow(majorColumn.toArray) //clear duplicate data
  println()
  println()
  println()


  val maleMajors = getRows(search("Male", studentData(2)), studentData, "Male") //get all male students
  printDF(maleMajors)


  val femaleStudents = getRows(search("Female", studentData(2)), studentData, "Female") //get all female students
  printDF(femaleStudents)


  val maleAgeAverage = sumColumn(search("Age", studentData.head), 2, studentData, "Male") / count(maleMajors) //get male average
  println("Male Age Average: " + maleAgeAverage)
  println()
  println()


  val femaleAgeAverage = sumColumn(search("Age", studentData.head), 2, studentData, "Female") / count(femaleStudents) //get remale average
  println("Female Age Average: " + femaleAgeAverage)
  println()
  println()


  val studentsUnder22 = pull(studentData, "Age < 22", 2) //get all students under 22
  printDF(studentsUnder22)


  val descendingStudents = sort(search("Age", studentData.head), 2, studentData, "DESC") //descending age
  printDF(descendingStudents)


  val exam1 = getRows(search("exam1", examData(1)), examData, "exam1")
  val exam2 = getRows(search("exam2", examData(1)), examData, "exam2")
  val exam3 = getRows(search("exam3", examData(1)), examData, "exam3") //get the exam scores
  printDF(exam1)
  printDF(exam2)
  printDF(exam3)


  val exam1Ave = sumColumn(2, 0, exam1) / count(exam1)
  val exam2Ave = sumColumn(2, 0, exam2) / count(exam2) //get all exams averages
  val exam3Ave = sumColumn(2, 0, exam3) / count(exam3)
  println("Exam 1 Average: " + exam1Ave)
  println("Exam 2 Average: " + exam2Ave)
  println("Exam 3 Average: " + exam3Ave)
  println()
  println()

  val exam1Max = pull(2, exam1, "MAX")
  val exam2Max = pull(2, exam2, "MAX") //get the max for the exam scores
  val exam3Max = pull(2, exam3, "MAX")
  printRow(exam1Max)
  printRow(exam2Max)
  printRow(exam3Max)
  println()
  println()

  val exam1Min = pull(2, exam1, "MIN")
  val exam2Min = pull(2, exam2, "MIN") //get the min for exam scores
  val exam3Min = pull(2, exam3, "MIN")
  printRow(exam1Min)
  printRow(exam2Min)
  printRow(exam3Min)
  println()
  println()

  val exam1Desc = sort(2, exam1, "DESC")
  val exam2Desc = sort(2, exam2, "DESC") //list exam scores in descending order
  val exam3Desc = sort(2, exam3, "DESC")
  var fullSortedExams = combine(exam1Desc, exam2Desc)
  fullSortedExams = combine(fullSortedExams, exam3Desc)
  printDF(fullSortedExams)

}
