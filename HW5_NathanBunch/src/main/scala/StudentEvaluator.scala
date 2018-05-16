import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class exam(score: Double)
case class student(name: String, age: Int, gender: String, exams: ArrayBuffer[exam])

object StudentEvaluator extends App {

  var students = ArrayBuffer[student]()
  var random = new Random()

  var firstName = Array("Addison","Adrian","Aiden","Ainsley","Alex","Amari","Andy","Ari","Ash","Aspen","Aubrey","August","Avery","Bailey","Bay","Blaine","Blake","Bobbie","Brett","Brook","Brooklyn","Caelan","Cameron","Campbell","Carroll","Carson","Casey","Charlie","Chris","Clay","Corey","Dana","Dakota","Dale","Dallas","Daryl","Delta","Devin","Dorian","Drew","Dylan","Easton","Eddie","Eli","Elliott","Emerson","Emery","Finley","Frances","Frankie","Gabriel","Glenn","Gray","Harley","Harper","Hayden","Hudson","Hunter","James","Jamie","Jayden","Jean","Jesse","Jordan","Jules","Julian","Kaden","Kai","Karter","Kelly","Kelsey","Kendall","Kennedy","Kyle","Lake","Landry","Lincoln","Logan","London","Lou","Mackenzie","Mason","Max","Maxwell","Monroe","Morgan","Parker","Pat","Peyton","Phoenix","Quinn","Ray","Reagan","Reed","Reese","Remy","Riley","River","Roan","Rory","Rowan","Rudy","Ryan","Sage","Sam","Sawyer","Shawn","Sean","Skylar","Spencer","Stevie","Sydney","Tanner","Tatum","Taylor","Toby","Tyler","Val","West","Winter")
  var lastName = Array("Davis", "Sholksteim", "Ruth", "Smith", "Shard", "von-Devoy", "Berton", "Simpson", "Garfield", "Beloy","Smith","Johnson","Williams","Jones","Brown","Davis","Miller","Wilson","Moore","Taylor","Anderson","Thomas","Jackson","White","Harris","Martin","Thompson","Garcia","Martinez","Robinson","Clark","Rodriguez","Lewis","Lee","Walker","Hall","Allen","Young","Hernandez","King","Wright","Lopez","Hill","Scott","Green","Adams","Baker","Gonzalez","Nelson","Carter","Mitchell","Perez","Roberts","Turner","Phillips","Campbell","Parker","Evans","Edwards","Collins","Stewart","Sanchez","Morris","Rogers","Reed","Cook","Morgan","Bell","Murphy","Bailey","Rivera","Cooper","Richardson","Cox","Howard","Ward","Torres","Peterson","Gray","Ramirez","James","Watson","Brooks","Kelly","Sanders","Price","Bennett","Wood","Barnes","Ross","Henderson","Coleman","Jenkins","Perry","Powell","Long","Patterson","Hughes","Flores","Washington","Butler","Simmons","Foster","Gonzales","Bryant","Alexander","Russell","Griffin","Diaz","Hayes")

  def round(num: Double): Double = {
    BigDecimal(num).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
  def getExamGrade(ranGen: Random): Double ={
    round(ranGen.nextDouble() * 100)
  }
  def getGender(ranGen: Random): String = {
    if (ranGen.nextDouble() * 10  <  5){
      "Male"
    } else {
      "Female"
    }
  }
  def getName(ranGen: Random, first: Array[String], last: Array[String]): String = {
    var fname = ranGen.nextInt(first.length-1)
    var lname = ranGen.nextInt(last.length-1)
    first(fname) + " " + last(lname)
  }
  def getAge(ranGen: Random): Int = {
    var age = 0
    if (ranGen.nextDouble()*100 < 60){
      if (ranGen.nextDouble()*100 < 80){
        age = ranGen.nextInt(25)
      } else {
        age = ranGen.nextInt(25) + 26
      }
    }else {
      if (ranGen.nextDouble()*100 < 80){
        age = ranGen.nextInt(25) + 26
      } else {
        age = ranGen.nextInt(25) + 50
      }
    }
    if (age < 17){
      age += 16
      age
    } else {
      age
    }
  }

  for(i <- 0 to 10){
    var exams = ArrayBuffer[exam]()
    for(i <- 0 to 2){
      exams += exam(getExamGrade(random))
    }
    students += student(getName(random, firstName, lastName), getAge(random), getGender(random), exams)
  }

  /*
   average grades of each student,
   average grades of female students,
   average grades of male students,
   maximum and minimum scores from all three exams (not from each exam),
   average ages of female students,
   average ages of male students.
   */

  def printStudents(students: Array[student]) {
    for (currentStudent <- students){
      var examNum = 1
      println(currentStudent.name + " " + currentStudent.age + " " + currentStudent.gender)
      for (examGrade <- currentStudent.exams){
        print("Exam " + examNum + ": " + examGrade.score + " ")
        examNum += 1
      }
      println()
      examNum = 1
      println()
    }
  }

  def averageGrades(students: Array[student]): Double = {
    var gradeTotal = 0.0
    for(cstudent <- students){
      cstudent match {
        case student(_, _, _, exams) => {
          for(grade <- exams){
            gradeTotal += grade.score
          }
        }
        case _ => println("Woat?")
      }
    }
    gradeTotal / (students.length*3)
  }

  def averageGradesFemale(students: Array[student]): Double = {
    var gradeTotal = 0.0
    var count = 0
    for(cstudent <- students){
      cstudent match {
        case student(_, _, gender, exams) => if (gender.equals("Female")){
          for(grade <- exams){
            gradeTotal += grade.score
            count += 1
          }
        }
        case _ => println("Not female student.")
      }
    }
    gradeTotal / count
  }

  def averageGradesMale(students: Array[student]): Double = {
    var gradeTotal = 0.0
    var count = 0
    for(cstudent <- students){
      cstudent match {
        case student(_, _, gender, exams) => if (gender.equals("Male")){
          for(grade <- exams){
            gradeTotal += grade.score
            count += 1
          }
        }
        case _ => println("Not male student.")
      }
    }
    gradeTotal / count
  }

  def checkExam(grades: Array[exam], compare: Double, testType: String): Boolean = {
    var ans = false
    for (grade <- grades){
      if (grade.score < compare && testType.equals("min")){
        ans = true
      }

      if (grade.score > compare && testType.equals("max")){
        ans = true
      }
    }
    ans
  }

  def maxMinExams(students: Array[student]): (Double, Double) = {
    var max = 0.0
    var min = 100.0
    for(cstudent <- students){
      cstudent match {
        case student(_, _, _, exams) if(checkExam(exams.toArray, min, "min")) => {
          for(grade <- exams){
            if (min > grade.score)
              min = grade.score
          }
        }
        case student(_, _, _, exams) if(checkExam(exams.toArray, max, "max")) => {
          for(grade <- exams){
            if (max < grade.score)
              max = grade.score
          }
        }
        case _ => {}
      }
    }
    (min, max)
  }

  def averageAgeFemale(students: Array[student]): Double = {
    var ageTotal = 0
    var count = 0
    for(cstudent <- students){
      cstudent match {
        case student(_, age, gender, _) => if (gender.equals("Female")){
          ageTotal += age
          count += 1
        }
        case _ => println("Not female student.")
      }
    }
    ageTotal*1.0 / count*1.0
  }

  def averageAgeMale(students: Array[student]): Double = {
    var ageTotal = 0
    var count = 0
    for(cstudent <- students){
      cstudent match {
        case student(_, age, gender, _) => if (gender.equals("Male")){
          ageTotal += age
          count += 1
        }
        case _ => println("Not male student.")
      }
    }
    ageTotal*1.0 / count*1.0
  }


  printStudents(students.toArray)
  println("Average Grades: " + round(averageGrades(students.toArray)))
  println("Average Grades Female: " + round(averageGradesFemale(students.toArray)))
  println("Average Grades Male: " + round(averageGradesMale(students.toArray)))
  println("Min, Max Grades: " + maxMinExams(students.toArray))
  println("Average Ages Female: " + round(averageAgeFemale(students.toArray)))
  println("Average Ages Male: " + round(averageAgeMale(students.toArray)))

}
