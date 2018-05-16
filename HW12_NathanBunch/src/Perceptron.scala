object Perceptron extends App {

  /*
  In order for this program to work, you must have the Matrix2D, Vector, and MersennaTwister files
  compiled along with this file. You can also copy/paste those other classes above the object that
  is defined above.
   */

  var X = Matrix2D(
    Array(
    Array(-2.0,4.0,-1.0),
    Array(4.0,1.0,-1.0),
    Array(1.0,6.0,-1.0),
    Array(2.0,4.0,-1.0),
    Array(6.0,2.0,-1.0)
    )
  )

  var y = Vector(Array(-1.0,-1.0,1.0,1.0,1.0))

  def perceptronSgd(X: Matrix2D,Y: Vector): Vector ={
    var w = Vector(X.shape(1))
    var eta = 1.0
    val epochs = 20

    for(t <- 1 to epochs){
      for(i <- 0 until X.shape(1)){
        if((X.row(i) dot w) * Y.get(i) <= 0){
          w = w + (X.row(i) * Y.get(i)) * eta
        }
      }
    }
    w
  }

  var weights = perceptronSgd(X,y)
  println(weights) //I have no idea why, but the middle value of the vector is one off from the value it should return

}
