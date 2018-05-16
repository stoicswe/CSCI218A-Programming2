import scala.collection.mutable.ArrayBuffer

class Vector(private val vector: Array[Double]){
  private var isTranspose = false //in case this has to be implemented at some point
  def getVector = vector

  def +(other: Vector): Vector ={
    var newVector = ArrayBuffer[Double]()
    var otherVector = other.getVector
    if(vector.length != otherVector.length){throw new Exception("Vectors cannot be added! [" + vector.length + " != " + otherVector.length)}
    for(i <- 0 until otherVector.length){
      newVector += vector(i) + otherVector(i)
    }
    Vector(newVector.toArray)
  }

  def *(other: Double): Vector ={
    var newVector = ArrayBuffer[Double]()
    for(num <- vector){
      newVector += num * other
    }
    Vector(newVector.toArray)
  }

  def get(index: Int): Double ={
    vector(index)
  }

  def unary_*(other: Double): Vector ={
    var newVector = ArrayBuffer[Double]()
    for(num <- vector){
      newVector += num * other
    }
    Vector(newVector.toArray)
  }

  def dot(other: Vector): Double ={
    var tempVals = ArrayBuffer[Double]()
    var otherVector = other.vector
    if(vector.length != otherVector.length){throw new Exception("Vector dot cannot be performed! [" + vector.length + " != " + otherVector.length + "]")}
    for(i <- 0 until otherVector.length) {
      tempVals += vector(i) * otherVector(i)
    }
    tempVals.sum
  }

  override def toString: String = {
    var returnString = "[ "
    for(num <- vector){
      returnString += num.toString + " "
    }
    returnString += "]"
    returnString
  }
}

object Vector {
  def apply(value: Array[Double]) = new Vector(value)
  def apply(value: Int) = new Vector(Array.fill[Double](value)(0))
}

