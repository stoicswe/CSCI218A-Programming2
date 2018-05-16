class Pizza(var price: Double) {
  def unary_+(){
    this.price = BigDecimal(this.price + this.price*0.05).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
  def unary_-(){
    this.price = BigDecimal(this.price - this.price*0.05).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
  override def toString: String = "pizza price: " + price
}
object Pizza {
  def apply(price: Double) = new Pizza(price)
}

object PizzaPies extends App {
  var myPizza = Pizza(3.14)
  println(myPizza)
  +myPizza
  println(myPizza)
  -myPizza
  -myPizza
  println(myPizza)
}
