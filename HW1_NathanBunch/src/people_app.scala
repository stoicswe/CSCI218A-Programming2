import people._

object people_app extends App {

  val person = new Person()
  println("person's name: " + person.name)
  val person1 = new Person("Wei")
  println("person1's name: " +person1.name)
  val student = new Student("Hu", 3.4)
  println("student's name: " + student.name)
  println("student's GPA: " +student.GPA)
  val college_student = new CollegeStudent("Mike", 3.8, "Computer Science")
  println("college_student's name: " + college_student.name)
  println("college_student's GPA: " +college_student.GPA)
  println("college_student's major: " +college_student.major)
}

