class Point(var x :Int, var y :Int) {
  def move(mx :Int, my: Int){ this.x = mx; this.y = my;}
  override def toString: String = "(" + x + ", " + y + ")"
}

object Point{
  def apply(x: Int, y: Int): Point ={
    return new Point(x, y)
  }
}

class Circle(val radius: Double){
  def area = radius * radius * Math.PI
}

class Cylinder(override val radius: Double, height: Double) extends Circle(radius){
  def vol = height * area
}

object Circle{
  def apply(radius: Double): Circle ={
    return new Circle(radius)
  }
}

object Cylinder{
  def apply(radius: Double, height: Double): Circle ={
    return new Cylinder(radius, height)
  }
}

object classExample extends App {
  var myPoint = new Point(3,5)
  var myPoint2 = Point(4,6)
  var myCircle = new Circle(2.3)
  var myCircle2 = Circle(5.6)
  var myCylinder = new Cylinder(2,7.8)
  var myCylinder2 = Cylinder(1.2,6.7)
  println(myPoint.toString)
  println(myPoint2.toString)
  myPoint.move(50, 34)
  myPoint2.move(33,11)
  println(myPoint.toString)
  println(myPoint2.toString)
}
