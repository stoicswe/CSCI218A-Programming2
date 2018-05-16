import scala.collection.mutable.ArrayBuffer

abstract class item
case class storeItem(name: String, price: Double) extends item{
  val serialNumber = storeItem.newSerial()
  def getPrice = price
}
object storeItem{
  private var lastNumber = 0
  private def newSerial () = {lastNumber += 1; lastNumber}
}
case class weightItem(name: String, price: Double, weight: Double) extends item {
  def getWeight = weight
  def getTotalPrice = price*weight
}
case class unitItem(name: String, price: Double, weight: Double, description: String, numberOf: Int) extends item{
  def getDescription = description
  def getPrice = (price*numberOf)*weight
}
case class preparedItem(name: String, price: Double, weight: Double, description: String, discount: Double, numberOf: Int) extends item{
  def getDiscount = discount
  def getTotalPrice = numberOf*(price*weight) - price*discount
}

object BundlesCalculator extends App {
  var myCart = new ArrayBuffer[item]

  myCart += storeItem("action figure", 4.5)
  myCart += storeItem("puzzle", 3.0)
  myCart += weightItem("bag of peanuts", 0.2, 10)
  myCart += unitItem("lego set", 0.05, 20, "Qunatum Computer", 500)
  myCart += preparedItem("sushi", 3, 0.12, "Salmon Sushi", 0.02, 50)

  myCart += storeItem("rubiks cube", 12.50)
  myCart += storeItem("doughnut", 3.5)
  myCart += weightItem("brussle sprouts", 0.05, 10)
  myCart += unitItem("Build it Yourself Computer Kit", 10, 50, "Programmable Desktop", 30)
  myCart += preparedItem("fudge", 2.5, 0.04, "Dark Chocolate Fudge", 0.0, 25)

  for(currentItem <- myCart){
    currentItem match {
      case x: storeItem => println("Name: " + x.name + " |Price: " + x.price)
      case y: weightItem => println("Name: " + y.name + " |Price: " + y.getTotalPrice + " |Weight: " + y.weight)
      case z: unitItem => println("Name: " + z.name + " |Price: " + z.getPrice + " |Weight: " + z.weight + " |Description: " + z.description + " |Number of Items: " + z.numberOf)
      case w: preparedItem => println("Name: " + w.name + " |Price: " + w.getTotalPrice + " |Weight: " + w.weight + " |Description: " + w.description + " |Number of Items: " + w.numberOf)
    }
  }
}
