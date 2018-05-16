package people

class CollegeStudent extends Student {
  var major = ""
  def this(name: String){
    this()
    this.name = name
  }
  def this(name: String, gpa: Double){
    this()
    this.name = name
    this.GPA = gpa
  }
  def this(name: String, gpa: Double, major: String){
    this()
    this.name = name
    this.GPA = gpa
    this.major = major
  }
}
