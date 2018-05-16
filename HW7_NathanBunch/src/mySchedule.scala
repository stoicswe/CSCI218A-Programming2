import scala.collection.mutable.ArrayBuffer

class schedule() {
  private var mycourses = ArrayBuffer("Programming I")
  val addToMySchedule = (course: String) => {
    mycourses += course
    println(mycourses.mkString(", "))
  }
  def remove(i: Int){
    val rem = mycourses(i)
    mycourses match {
      case ArrayBuffer(b, x, rest @ _*) => if (x == rem) {
        val t = b.mkString +: rest
        mycourses = ArrayBuffer(t : _*)
      }
      case _ => println("Data not found.")
    }
    println(mycourses.mkString(", "))
  }

  def clean(){
    for(i <- 0 until(mycourses.length-1))
      if (mycourses(i).equals("")){
        mycourses.remove(i)
      }
    println(mycourses.mkString(", "))
  }
}

object schedule{
  def apply() = new schedule()
}

object mySchedule extends App {
  var mySchedule = schedule()
  mySchedule.addToMySchedule("Programming II")
  mySchedule.addToMySchedule("Programming III")
  mySchedule.remove(1)
  mySchedule.addToMySchedule("Quantum Computing")
  mySchedule.remove(1)
  mySchedule.addToMySchedule("Networking")
  mySchedule.addToMySchedule("")
  mySchedule.addToMySchedule("Datascience I")
  mySchedule.clean()
}
