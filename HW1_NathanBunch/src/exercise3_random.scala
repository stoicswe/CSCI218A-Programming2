import Random.Random

object exercise3_random extends App {
  var myRandom = new Random(18743)
  println(myRandom.nextInt(50))
  println(myRandom.nextDouble(5.9))
}
