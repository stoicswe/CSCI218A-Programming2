object ExtraCredit extends App {

  var matrix = Matrix2D(Array(Array(1.0,2.0), Array(3.0,4.0)))
  var vector = Vector(Array(1.0,2.0))
  println(matrix * matrix)
  println()
  println(matrix * vector)
  println()
  println(matrix * 2.0)
  println()
}
