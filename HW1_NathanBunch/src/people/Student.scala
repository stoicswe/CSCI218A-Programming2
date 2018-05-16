package people

class Student extends Person {
  var GPA = 0.0
  def this(name: String){
    this()
    this.name = name
  }
  def this(name: String, gpa : Double){
    this()
    this.name = name
    this.GPA = gpa
  }
}
