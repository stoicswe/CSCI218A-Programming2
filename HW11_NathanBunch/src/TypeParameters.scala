import scala.collection.generic._
import scala.collection.mutable.ArrayBuffer

class Fruit(val name: String, val price: Double, val weight: Double) {
  def getName = name
  def getPrice = price
  def getWeight = weight
}

class Apple (override val name: String = "apple", override val price: Double = 1.5, override val weight: Double = 85.3) extends Fruit (name, price, weight) {
  override def toString: String = "(" + name + ", $" + price + ", " + weight + "g)"
}

class Banana (override val name: String = "banana", override val price: Double = 2.0, override val weight: Double = 125.7) extends Fruit (name, price, weight) {
  override def toString: String = "(" + name + ", $" + price + ", " + weight + "g)"
}

class Orange (override val name: String = "orange", override val price: Double = 1.75, override val weight: Double = 131.2) extends Fruit (name, price, weight) {
  override def toString: String = "(" + name + ", $" + price + ", " + weight + "g)"
}

class Container[T <: Fruit] {
  private var itemArray = ArrayBuffer[T]()

  def add(value: T): Unit ={
    itemArray += value
  }

  def remove(index: Int): Unit ={
    itemArray.remove(index)
  }

  def sortByName(): Unit ={
    for(i <- 0 until itemArray.length){
      var minPOS = i
      for (j <- i+1 until itemArray.length){
        if (itemArray(j).getName < itemArray(minPOS).getName){
          minPOS = j
        }

        if (minPOS != i){
          val temp = itemArray(i)
          itemArray(i) = itemArray(minPOS)
          itemArray(minPOS) = temp
        }
      }
    }
  }

  def sortByPrice(): Unit ={
    for(i <- 0 until itemArray.length){
      var minPOS = i
      for (j <- i+1 until itemArray.length){
        if (itemArray(j).getPrice < itemArray(minPOS).getPrice){
          minPOS = j
        }

        if (minPOS != i){
          val temp = itemArray(i)
          itemArray(i) = itemArray(minPOS)
          itemArray(minPOS) = temp
        }
      }
    }
  }

  def sortByWeight(): Unit ={
    for(i <- 0 until itemArray.length){
      var minPOS = i
      for (j <- i+1 until itemArray.length){
        if (itemArray(j).getWeight < itemArray(minPOS).getWeight){
          minPOS = j
        }

        if (minPOS != i){
          val temp = itemArray(i)
          itemArray(i) = itemArray(minPOS)
          itemArray(minPOS) = temp
        }
      }
    }
  }

  override def toString: String = {
    var returnString = ""
    for(item <- itemArray){
      returnString += item.toString + " "
    }
    returnString
  }
}

object TypeParameters extends App{
  var myBasket = new Container[Fruit]
  println("New Basket")
  myBasket.add(new Orange())
  myBasket.add(new Apple())
  myBasket.add(new Banana())
  println(myBasket)
  println("Adding orange")
  myBasket.add(new Orange())
  println(myBasket)
  println("Removing orange")
  myBasket.remove(3)
  println(myBasket)
  println("Adding another banana")
  myBasket.add(new Banana())
  println(myBasket)
  println("Sorting the basket by name")
  myBasket.sortByName()
  println(myBasket)
  println("Sorting by price")
  myBasket.sortByPrice()
  println(myBasket)
  println("Sorting by weight")
  myBasket.sortByWeight()
  println(myBasket)
}
